-module(vera_client).

-export([start_link/1, start_link/2]).
-export([init/1, handle_call/3]).
-export([device_list/1, device_id/2, device_vars/2, device_var_key/3, device_name/2, device_delete/2]).
-export([device_power/3, device_dim/3]).
-export([scene_list/1, scene_id/2, scene/2]).
-export([room_list/1]).
-export([job/2, reload/1, alive/1]).
-export([auth/2]).

-record(state, {host, session, pkid}).

start_link(Host) when is_list(Host) ->
    start_link({local, Host});
start_link(Connection) when is_tuple(Connection) ->
    gen_server:start_link(?MODULE, Connection, []).
start_link(Name, Host) when is_list(Host) ->
    start_link(Name, {local, Host});
start_link(Name, Connection) when is_tuple(Connection) ->
    gen_server:start_link(Name, ?MODULE, Connection, []).

call(PID, MSG) ->
    case gen_server:call(PID, MSG, infinity) of
        {ok, Reply} ->
            Reply;
        {error, _} = E ->
            E
    end.

auth(PID, Auth) ->
    call(PID, {auth, Auth}).

reload(PID) ->
    call(PID, reload).

alive(PID) ->
    call(PID, alive).

job(PID, ID) ->
    call(PID, {job, ID}).

device_list(PID) ->
    call(PID, device_list).

device_power(PID, Device, P) ->
    call(PID, {device_power, Device, P}).

device_dim(PID, Device, Dim) when is_integer(Dim) ->
    call(PID, {device_dim, Device, Dim}).

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
    call(PID, {device_vars, ID}).

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
    end;
device_var_key(_PID, _ID, _Key, {error, _} = E) ->
    E.

device_delete(PID, ID) when is_integer(ID) ->
    call(PID, {device_delete, ID}).

scene_list(PID) ->
    call(PID, scene_list).

room_list(PID) ->
    call(PID, room_list).

scene_id(PID, Name) when is_list(Name) ->
    scene_id(PID, list_to_binary(Name));
scene_id(PID, Name) when is_binary(Name) ->
    case scene_list(PID) of
        SL when is_list(SL) ->
            case lists:keysearch(Name, 2, SL) of
                false ->
                    undefined;
                {value, {ID, Name}} ->
                    ID
            end;
        {error, _} = E ->
            E
    end.

scene(PID, ID) when is_integer(ID) ->
    gen_server:call(PID, {scene, ID}).

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
            end;
        {error, _} = E ->
            {reply, E, State}
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
            end;
        {error, _} = E ->
            {reply, E, State}
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
            end;
        {error, _} = E ->
            {reply, E, State}
    end;
handle_call({device_dim, ID, Dim}, _From, State) ->
    PL = [
          {"id", "action"},
          {"DeviceNum", integer_to_list(ID)},
          {"serviceId", "urn:upnp-org:serviceId:Dimming1"},
          {"action", "SetLoadLevelTarget"},
          {"newLoadlevelTarget", integer_to_list(Dim)}
         ],
    case hit_vera(PL, State) of
        PL1 when is_list(PL1) ->
            case proplists:get_value(<<"u:SetLoadLevelTargetResponse">>, PL1) of
                [{<<"OK">>,<<"OK">>}] ->
                    {reply, {ok, ok}, State}
            end;
        {error, _} = E ->
            {reply, E, State}
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
                        {undefined, <<"OK">>} -> 
                            {reply, {ok, ok}, State};
                        {Job, undefined} when is_binary(Job) ->
                            {reply, {ok, list_to_integer(binary_to_list(Job))}, State}
                    end
            end;
        {error, _} = E ->
            {reply, E, State}
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
            {reply, {ok, F(proplists:get_value(<<"status">>, PL))}, State};
        {error, _} = E ->
            {reply, E, State}
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
            end;
        {error, _} = E ->
            {reply, E, State}
    end;
handle_call(alive, _From, State) ->
    PL = [
          {"id", "alive"}
         ],
    case hit_vera(PL, State) of
        ok ->
            {reply, {ok, ok}, State};
        {error, _} = E ->
            {reply, E, State}
    end;
handle_call({scene, ID}, _From, State) ->
    PL = [
          {"id", "action"},
          {"serviceId", "urn:micasaverde-com:serviceId:HomeAutomationGateway1"},
          {"action", "RunScene"},
          {"SceneNum",integer_to_list(ID)}
         ],
    case hit_vera(PL, State) of
        ok ->
            {reply, {ok, ok}, State};
        {error, _} = E ->
            {reply, E, State}
    end;
handle_call(reload, _From, State) ->
    PL = [
          {"id", "reload"}
         ],
    case hit_vera(PL, State) of
        ok ->
            {reply, {ok, ok}, State};
        {error, _} = E ->
            {reply, E, State}
    end;
handle_call({device_delete, ID}, _From, State) ->
    PL = [
          {"id", "device"},
          {"action", "delete"},
          {"device", integer_to_list(ID)}
         ],
    case hit_vera(PL, State) of
        ok ->
            {reply, {ok, ok}, State};
        {error, _} = E ->
            {reply, E, State}
    end.

hit_vera(Suffix, #state{pkid = undefined, host = Host}) ->
    veraerl_util:local_vera(Host, Suffix);
hit_vera(Suffix, #state{pkid=PKID, host=Host, session=Session}) ->
    veraerl_util:remote_vera(Host, Suffix, Session, PKID).

