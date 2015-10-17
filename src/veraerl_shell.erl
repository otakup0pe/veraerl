-module(veraerl_shell).
-behaviour(shellbeam).

-export([commands/0]).
-export([discover/0, start/2]).

commands() ->
    [
     {["device"], "Device commands", {subshell, [veraerl_device_shell], "device"}},
     {["scene"], "Scene commands", {subshell, [veraerl_scene_shell], "scene"}},
     {["discover"], "Discover local veras", fun discover/0},
     {["start", {"name", auto}, {"host", auto}], "Start a vera client", fun start/2}
    ].

discover() ->
    F = fun(D) ->
                binary_to_list(proplists:get_value(ip, D))
        end,
    {ok, lists:map(F, veraerl_util:discover())}.

start(Name, Host) ->
    veraerl:start_child(Host, Name),
    {ok, "Started ~p", [Name]}.
