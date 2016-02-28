-module(vera_client).

-export([start_link/1, start_link/2]).
-export([init/1, handle_cast/2, handle_call/3]).
-export([device_list/1, device_power/3, device_id/2, device_vars/2, device_var_key/3, device_name/2]).
-export([scene_list/1, scene_id/2, scene/2]).
-export([room_list/1]).

-export([job/2, reload/1, alive/1]).

-record(state, {host, session, pkid}).

start_link(Host) when is_list(Host) ->
    start_link({local, Host});
start_link(Connection) when is_tuple(Connection) ->
    gen_server:start_link(?MODULE, Connection, []).
start_link(Name, Host) when is_list(Host) ->
    start_link(Name, {local, Host});
start_link(Name, Connection) when is_tuple(Connection) ->
    gen_server:start_link(Name, ?MODULE, Connection, []).

reload(PID) ->
    gen_server:cast(PID, reload).

alive(PID) ->
    gen_server:call(PID, alive).

job(PID, ID) ->
    {ok, R} = gen_server:call(PID, {job, ID}),
    R.

device_list(PID) ->
    {ok, R} = gen_server:call(PID, device_list),
    R.

device_power(PID, Device, P) ->
    {ok, R} = gen_server:call(PID, {device_power, Device, P}),
    R.

device_id(PID, Name) when is_list(Name) ->
    device_id(PID, list_to_binary(Name));
device_id(PID, Name) when is_binary(Name) ->
    device_id(PID, Name, device_list(PID), []).
device_id(_PID, _Name, [], [I]) when is_integer(I) ->
    I;
device_id(_PID, _Name, [], R) when is_list(R) ->
    {multiple, R};
device_id(PID, Name, [{ID, Name}|T], R) ->
    device_id(PID, Name, T, [ID|R]);
device_id(PID, Name, [_|T], R) ->
    device_id(PID, Name, T, R).

device_name(PID, ID) when is_pid(PID), is_integer(ID) ->
    device_name(ID, device_list(PID));
device_name(ID, [{ID, Name}|_T]) ->
    Name;
device_name(ID, [_|T]) ->
    device_name(ID, T);
device_name(_ID, []) ->
    undefined.

device_vars(PID, ID) when is_integer(ID) ->
    {ok, R} = gen_server:call(PID, {device_vars, ID}),
    R.

device_var_key(PID, ID, Key) ->
    device_var_key(PID, ID, Key, device_vars(PID, ID)).
device_var_key(_PID, _ID, _Key, []) ->
    {error, badarg};
device_var_key(PID, ID, Key, [PL|T]) ->
    case proplists:get_value(<<"variable">>, PL) of
        undefined ->
            device_var_key(PID, ID, Key, T);
        Key ->
            proplists:get_value(<<"value">>, PL);
        _ ->
            device_var_key(PID, ID, Key, T)
    end.

scene_list(PID) ->
    {ok, R} = gen_server:call(PID, scene_list),
    R.

room_list(PID) ->
    {ok, R} = gen_server:call(PID, room_list),
    R.

scene_id(PID, Name) when is_list(Name) ->
    scene_id(PID, list_to_binary(Name));
scene_id(PID, Name) when is_binary(Name) ->
    case lists:keysearch(Name, 2, scene_list(PID)) of
        false ->
            undefined;
        {value, {ID, Name}} ->
            ID
    end.

scene(PID, ID) when is_integer(ID) ->
    gen_server:cast(PID, {scene, ID}).

init({local, Host}) ->
    {ok, #state{host = Host}};
init({remote, RelayServer, PKID, Session}) ->
    {ok, #state{host = RelayServer, session = Session, pkid = PKID}}.

handle_call(device_list, _From, State) ->
    case hit_vera([{"id", "sdata"}], State) of
        PL when is_list(PL) ->
            case proplists:get_value(<<"devices">>, PL) of
                Dev when is_list(Dev) ->
                    F0 = fun(K0) when is_list(K0) ->
                                 F = fun(K) ->
                                             proplists:get_value(K, K0)
                                     end,
                                 {F(<<"id">>), F(<<"name">>)}
                         end,
                    {reply, {ok, lists:map(F0, Dev)}, State}
            end
    end;
handle_call(scene_list, _From, State) ->
    case hit_vera([{"id", "sdata"}], State) of
        PL when is_list(PL) ->
            case proplists:get_value(<<"scenes">>, PL) of
                Scene when is_list(Scene) ->
                    F0 = fun(K0) when is_list(K0) ->
                                 F = fun(K) ->
                                             proplists:get_value(K, K0)
                                     end,
                                 {F(<<"id">>), F(<<"name">>)}
                         end,
                    {reply, {ok, lists:map(F0, Scene)}, State}
            end
    end;
handle_call(room_list, _From, State) ->
    case hit_vera([{"id", "sdata"}], State) of
        PL when is_list(PL) ->
            case proplists:get_value(<<"rooms">>, PL) of
                Room when is_list(Room) ->
                    F0 = fun(K0) when is_list(K0) ->
                                 F = fun(K) ->
                                             proplists:get_value(K, K0)
                                     end,
                                 {F(<<"id">>), F(<<"name">>)}
                         end,
                    {reply, {ok, lists:map(F0, Room)}, State}
            end
    end;

handle_call({device_power, ID, P}, _From, State) ->
    PL = [
          {"id", "action"},
          {"DeviceNum", integer_to_list(ID)},
          {"serviceId", "urn:upnp-org:serviceId:SwitchPower1"},
          {"action", "SetTarget"},
          {"newTargetValue", integer_to_list(if P == on ->
                                                     1;
                                                P == off ->
                                                     0
                                             end)}],
    case hit_vera(PL, State) of
        PL1 when is_list(PL1) ->
            case proplists:get_value(<<"u:SetTargetResponse">>, PL1) of
                PL0 when is_list(PL0) ->
                    F = fun(K) ->
                                proplists:get_value(K, PL0)
                        end,
                    case {F(<<"JobID">>), F(<<"OK">>)} of
                        {undefined, <<"OK">>} -> ok;
                        {Job, undefined} when is_binary(Job) ->
                            {reply, {ok, list_to_integer(binary_to_list(Job))}, State}
                    end
            end
    end;
handle_call({job, ID}, _From, State) ->
    PL = [
          {"id", "jobstatus"},
          {"job", integer_to_list(ID)}
         ],
    case hit_vera(PL, State) of
        PL when is_list(PL) ->
            F = fun
                    (-1) -> undefined;
                    (0) -> waiting;
                    (1) -> running;
                    (2) -> error;
                    (3) -> terminated;
                    (4) -> done;
                    (5) -> callback;
                    (6) -> requeue;
                    (7) -> pending
                end,
            {reply, {ok, F(proplists:get_value(<<"status">>, PL))}, State}
    end;
handle_call({device_vars, ID}, _From, State) ->
    PL = [
          {"id","status"},
          {"DeviceNum", integer_to_list(ID)}
         ],
    case hit_vera(PL, State) of
        PL1 when is_list(PL1) ->
            case proplists:get_value(list_to_binary("Device_Num_" ++ integer_to_list(ID)), PL1) of
                PL0 when is_list(PL0) ->
                    case proplists:get_value(<<"states">>, PL0) of
                        V when is_list(V) ->
                            {reply, {ok, V}, State}
                    end
            end
    end;
handle_call(alive, _From, State) ->
    PL = [
          {"id", "alive"}
         ],
    case hit_vera(PL, State) of
        ok ->
            {reply, ok, State}
    end.

handle_cast({scene, ID}, #state{host=H} = State) ->
    PL = [
          {"id", "action"},
          {"serviceId", "urn:micasaverde-com:serviceId:HomeAutomationGateway1"},
          {"action", "RunScene"},
          {"SceneNum",integer_to_list(ID)}
         ],
    hit_vera(H, PL),
    {noreply, State};
handle_cast(reload, #state{host=H} = State) ->
    PL = [
          {"id", "reload"}
         ],
    hit_vera(H, PL),
    {noreply, State}.

hit_vera(Suffix, #state{pkid = undefined, host = Host}) ->
    veraerl_util:local_vera(Host, Suffix);
hit_vera(Suffix, #state{pkid=PKID, host=Host, session=Session}) ->
    veraerl_util:remote_vera(Host, Suffix, Session, PKID).
