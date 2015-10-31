-module(veraerl).

-export([main/1, start_link/0, start_child/1, start_child/2, pid/1]).
-export([init/1, handle_call/3, handle_info/2]).

-record(vera, {pid, name, host}).
-record(state, {veras=[]}).

main(_Commands) ->
    application:load(veraerl),
    veraerl_util:bootstrap(),
    block_until_done(user_drv:start(['tty_sl -c -e', {shellbeam, spawn_shell, [[magicbeam_shell, veraerl_shell], "Vera"]}])).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_child(Host) ->
    start_child(Host, Host).

start_child(Host, Name) ->
    {ok, _PID} = gen_server:call(?MODULE, {start_child, Host, Name}).

pid(Name) ->
    case gen_server:call(?MODULE, {pid, Name}) of
        {ok, PID} -> PID;
        {error, _} = E -> E
    end.

init([]) ->
    {ok, #state{}}.

handle_call({pid, Name}, _From, #state{veras = VC} = State) ->
    case lists:keysearch(Name, #vera.name, VC) of
        false ->
            {reply, {error, badarg}, State};
        {value, #vera{name = Name, pid = PID}} ->
            {reply, {ok, PID}, State}
    end;
handle_call({start_child, Host, Name}, _From, State) ->
    case p_start_child(Host, Name, State) of
        {PID, NewState} when is_pid(PID) ->
            {reply, {ok, PID}, NewState};
        {error, _} = E ->
            {reply, E, State}
    end.

p_start_child(Host, Name, #state{veras = VC} = State) ->
    case lists:keysearch(Name, #vera.name, VC) of
        false ->
            case supervisor:start_child(veraerl_child_sup, [Host]) of
                {ok, PID} ->
                    erlang:monitor(process, PID),
                    {PID, State#state{veras = [#vera{name=Name,host=Host,pid=PID}|VC]}}
            end;
        {value, {Name, _PID}} ->
            {{error, already_started}, State}
    end.

handle_info({'DOWN', _Ref, process, PID, _Info}, #state{veras = VC} = State) ->
    case lists:keysearch(PID, #vera.pid, VC) of
        false ->
            {noreply, State};
        {value, #vera{name=Name,host=Host}} ->
            {_PID, State0} = p_start_child(Host, Name, State#state{veras = lists:keydelete(PID, #vera.pid, VC)}),
            {noreply, State0}
    end.

block_until_done(PID) ->
    case is_process_alive(PID) of
        true ->
            timer:sleep(500),
            block_until_done(PID);
        false ->
            init:stop()
    end.
