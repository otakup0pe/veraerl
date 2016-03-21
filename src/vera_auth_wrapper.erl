-module(vera_auth_wrapper).

-export([start_link/3, init/1, handle_call/3, terminate/2]).

-record(state, {username, password, device, server, token, pid}).

start_link(Username, Password, Device) ->
    gen_server:start_link(?MODULE, {Username, Password, Device}, []).

init({Username, Password, Device}) ->
    {ok, p_rehash(#state{username = Username,
                         password = Password,
                         device = Device
                         })}.

p_rehash(State) ->
    p_rehash(State, 5).

p_rehash(_State, 0) ->
    erlang:exit({error, auth});
p_rehash(#state{username = U, password = P, device = D, pid = undefined} = State, I) ->
    case p_login(U, P, D) of
        {ok, RelayServer, RelaySession} ->
            case vera_client:start_link({remote, RelayServer, D, RelaySession}) of
                {ok, PID} ->
                    State#state{
                           server = RelayServer,
                           token = RelaySession,
                           pid = PID
                          }
            end;
        {error, auth} ->
            timer:sleep(5000),
            p_rehash(State, I - 1);
        {error, network} ->
            timer:sleep(5000),
            p_rehash(State, I - 1)
    end;
p_rehash(#state{username = U, password = P, device = D, pid = PID} = State, I) when is_pid(PID) ->
    case p_login(U, P, D) of
        {ok, RelayServer, RelaySession} ->
            ok = vera_client:auth(PID, {RelayServer, RelaySession}),
            State#state{server = RelayServer, token = RelaySession};
        {error, auth} ->
            timer:sleep(5000),
            p_rehash(State, I - 1);
        {error, network} ->
            timer:sleep(5000),
            p_rehash(State, I - 1)
    end.

p_login(U, P, D) ->
    case veraerl_util:login(U, P) of
        {ok, AT, AS, AccountSession} ->
            case p_relay_session(AT, AS, AccountSession, D) of
                {ok, RelayServer, RelaySession} ->
                    {ok, RelayServer, RelaySession};
                {error, _} = E ->
                    E
            end;
        {error, _} = E ->
            E
    end.

p_relay_session(AT, AS, AccountSession, D) ->
    {ok, DeviceURL, Account} = veraerl_util:decode_token(AT),
    case veraerl_util:discover(DeviceURL, Account, AccountSession) of
        Devices when is_list(Devices) ->
            case veraerl_util:device_session(D, AT, AS, Devices) of
                {ok, DeviceServer, DeviceSession} ->
                    {ok, _RelayServer, _RelaySession} = veraerl_util:relay_session(D, AT, AS, DeviceServer, DeviceSession)
            end;
        {error, _} = E ->
            E
    end.

handle_call(MSG, _From, State) ->
    case call_vera(MSG, State) of
        {ok, _} = Reply ->
            {reply, Reply, State}
    end.

call_vera(MSG, State) ->
    call_vera(MSG, State, 5).

call_vera(_MSG, _State, 0) ->
    erlang:exit({error, problems});
call_vera(MSG, #state{pid = PID} = State, I) when is_pid(PID) ->
    case gen_server:call(PID, MSG) of
        {error, auth} ->
            call_vera(MSG, p_rehash(State), I - 1);
        {error, _} = E ->
            E;
        {ok, _} = Reply ->
            Reply
    end.

terminate(_R, _State) ->
    ok.
