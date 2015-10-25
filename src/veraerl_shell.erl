-module(veraerl_shell).
-behaviour(shellbeam).

-export([commands/0]).
-export([discover/0, start/2, auto/0]).

commands() ->
    [
     {["device"], "Device commands", {subshell, [veraerl_device_shell], "device"}},
     {["scene"], "Scene commands", {subshell, [veraerl_scene_shell], "scene"}},
     {["discover"], "Discover local veras", fun discover/0},
     {["auto"], "Configure the first vera found on the LAN", fun auto/0},
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

auto() ->
    case veraerl_util:discover() of
        [] ->
            {ok, "No veras found", []};
        [H|_] ->
            case proplists:get_value(ip, H) of
                IP when is_binary(IP) ->
                    IPs = binary_to_list(IP),
                    veraerl:start_child(IPs, "auto"),
                    {ok, "Configured ~p as auto", [IPs]}
            end
    end.
