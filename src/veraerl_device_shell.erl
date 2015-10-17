-module(veraerl_device_shell).
-behaviour(shellbeam).

-export([commands/0]).
-export([device_list/1, device_on/2, device_off/2, device_vars/2]).

commands() ->
    [
     {["list", {"vera", auto}], "Device Listing", fun device_list/1},
     {["switch", {"vera", auto}, "on", {"name", string}], "Switch somethign on", fun device_on/2},
     {["switch", {"vera", auto}, "off", {"name", string}], "Switch something off", fun device_off/2},
     {["vars", {"vera", auto}, {"name", string}], "Device variables", fun device_vars/2}
    ].

device_list(Name) ->
    {ok, lists:map(fun({ID, DName}) ->
                          io_lib:format("#~4..0B ~p~n", [ID, binary_to_list(DName)])
                   end, vera_client:device_list(veraerl:pid(Name)))}.

device_off(Name, DName) ->
    vera_client:device_power(veraerl:pid(Name), vera_client:device_id(veraerl:pid(Name), list_to_binary(DName)), off),
    {ok, "Turned off"}.

device_on(Name, DName) ->
    vera_client:device_power(veraerl:pid(Name), vera_client:device_id(veraerl:pid(Name), list_to_binary(DName)), on),
    {ok, "Turned on"}.

device_vars(Name, DName) ->
    {ok, lists:map(fun(PL) ->
                           F = fun(K) ->
                                       string:strip(binary_to_list(proplists:get_value(K, PL)))
                               end,
                           Service = lists:sublist(string:tokens(F(<<"service">>), ":"), 4, 1),
                           Variable = F(<<"variable">>),
                           string:left(Service, 20) ++ string:left(Variable, 20) ++ F(<<"value">>) ++ "DONG\n"
                   end, vera_client:device_vars(veraerl:pid(Name), vera_client:device_id(veraerl:pid(Name), list_to_binary(DName))))}.
