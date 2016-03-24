-module(veraerl_device_shell).
-behaviour(shellbeam).

-export([commands/0]).
-export([device_list/1, device_on/2, device_off/2, device_vars/2]).
-export([device_name/2, device_id/2]).

commands() ->
    [
     {["list", {"vera", string}], "Device Listing", fun device_list/1},
     {["switch", {"vera", string}, "on", {"name", string}], "Switch something on", fun device_on/2},
     {["switch", {"vera", string}, "off", {"name", string}], "Switch something off", fun device_off/2},
     {["dim", {"vera", string}, {"name", string}, {"dim", integer}], "Device Dim", fun device_dim/3},
     {["vars", {"vera", string}, {"name", string}], "Device variables", fun device_vars/2},
     {["name", {"vera", string}, {"id", integer}], "Device name", fun device_name/2},
     {["id", {"vera", string}, {"name", string}], "Device ID", fun device_id/2},
     {["delete", {"vera", string}, {"id", integer}], "Delete device", fun device_delete/2}
    ].

device_dim(Name, DName, Dim) ->
    case veraerl_shell:autopid(Name) of
        {error, _} = E ->
            E;
        PID when is_pid(PID) ->
            ok = vera_client:device_dim(PID, p_device_id(PID, DName), Dim),
            {ok, "Adjusted dimmer"}
    end.
    
device_list(Name) ->
    case veraerl_shell:autopid(Name) of
        {error, _} = E ->
            E;
        PID when is_pid(PID) ->
            Devices = vera_client:device_list(PID),
            {ok, lists:map(fun({ID, DName}) ->
                                   io_lib:format("#~4..0B ~p~n", [ID, binary_to_list(DName)])
                           end, Devices)}
    end.

device_off(Name, DName) ->
    case veraerl_shell:autopid(Name) of
        {error, _} = E ->
            E;
        PID when is_pid(PID) ->
            vera_client:device_power(PID, p_device_id(PID, DName), off),
            {ok, "Turned off"}
    end.

device_on(Name, DName) ->
    case veraerl_shell:autopid(Name) of
        {error, _} = E ->
            E;
        PID when is_pid(PID) ->
            vera_client:device_power(PID, p_device_id(PID, DName), on),
            {ok, "Turned on"}
    end.

device_vars(Name, DName) ->
    DFun = fun(String) ->
                   string:left(String, 20) ++ " "
           end,
    case veraerl_shell:autopid(Name) of
        {error, _} = E ->
            E;
        PID when is_pid(PID) ->
            {ok, lists:map(fun(PL) ->
                                   F = fun(K) ->
                                               string:strip(binary_to_list(proplists:get_value(K, PL)))
                                       end,
                                   [Service] = lists:sublist(string:tokens(F(<<"service">>), ":"), 4, 1),
                                   Variable = F(<<"variable">>),
                                   DFun(Service) ++ DFun(Variable) ++ F(<<"value">>) ++ "\n"
                           end, vera_client:device_vars(PID, p_device_id(PID, DName)))}
    end.

p_device_id(PID, Str) when is_list(Str), is_pid(PID) ->
    case catch list_to_integer(Str) of
        I when is_integer(I) -> I;
        {'EXIT', {badarg, _}} ->
            vera_client:device_id(PID, list_to_binary(Str))
    end.

device_name(Name, Id) when is_integer(Id) ->
    case veraerl_shell:autopid(Name) of
        {error, E} ->
            E;
        PID when is_pid(PID) ->
            {ok, binary_to_list(vera_client:device_name(PID, Id))}
    end.

device_id(Name, Id) when is_integer(Id) ->
    case veraerl_shell:autopid(Name) of
        {error, E} ->
            E;
        PID when is_pid(PID) ->
            {ok, binary_to_list(vera_client:device_id(PID, Id))}
    end.

device_delete(Name, Id) when is_integer(Id) ->
    case veraerl_shell:autopid(Name) of
        {error, E} ->
            E;
        PID when is_pid(PID) ->
            ok = vera_client:device_delete(PID, Id),
            {ok, "Deleted"}
    end.
