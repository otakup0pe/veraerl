-module(veraerl).

-export([main/1, start_link/0, start_child/1, start_child/2, pid/1]).
-export([init/1, handle_call/3, handle_info/2]).

-record(state, {vera_client=[]}).

main(_Commands) ->
    application:load(veraerl),
    veraerl_util:bootstrap(),
    block_until_done(user_drv:start(['tty_sl -c -e', {shellbeam, spawn_shell, [[magicbeam_shell, veraerl_shell, veraerl_device_shell], "Vera"]}])).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_child(Host) ->
    start_child(Host, Host).

start_child(Host, Name) ->
    {ok, _PID} = gen_server:call(?MODULE, {start_child, Host, Name}).

pid(Name) ->
    {ok, PID} = gen_server:call(?MODULE, {pid, Name}),
    PID.

init([]) ->
    {ok, #state{}}.

handle_call({pid, Name}, _From, #state{vera_client = VC} = State) ->
    case lists:keysearch(Name, 1, VC) of
        false ->
            {reply, {error, badarg}, State};
        {value, {Name, PID}} ->
            {reply, {ok, PID}, State}
    end;
handle_call({start_child, Host, Name}, _From, #state{vera_client = VC} = State) ->
    case lists:keysearch(Name, 1, VC) of
        false ->
            case supervisor:start_child(veraerl_child_sup, [Host]) of
                {ok, PID} ->
                    erlang:monitor(process, PID),
                    {reply, {ok, PID}, State#state{vera_client = [{Name, PID}|VC]}}
            end;
        {value, {Name, _PID}} ->
            {reply, {error, already_started}, State}
    end.

handle_info({'DOWN', _Ref, process, PID, _Info}, #state{vera_client = VC} = State) ->
    case lists:member(PID, VC) of
        false ->
            {noreply, State};
        true ->
            {noreply, State#state{vera_client = lists:keyremove(PID, 2, VC)}}
    end.

block_until_done(PID) ->
    case is_process_alive(PID) of
        true ->
            timer:sleep(500),
            block_until_done(PID);
        false ->
            init:stop()
    end.
