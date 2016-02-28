-module(veraerl_shell).
-behaviour(shellbeam).

-export([autopid/1]).
-export([commands/0]).
-export([discover/0, start/2, auto/0, reload/1]).

autopid(Name) when is_list(Name) ->
    case veraerl:pid(Name) of
        PID when is_pid(PID) ->
            PID;
        {error, badarg} ->
            {error, "Invalid Vera name - did you start it?"}
    end.
                
commands() ->
    [
     {["device"], "Device commands", {subshell, [veraerl_device_shell], "device"}},
     {["scene"], "Scene commands", {subshell, [veraerl_scene_shell], "scene"}},
     {["discover"], "Discover local Veras", fun discover/0},
     {["auto"], "Configure the first Vera found on the LAN", fun auto/0},
     {["reload", {"name", string}], "Reload a Vera", fun reload/1},
     {["alive", {"name", string}], "Check a Vera heartbeat", fun alive/1},
     {["start", {"name", string}, {"host", string}], "Start a Vera client", fun start/2}
    ].

reload(Name) ->
    case autopid(Name) of
        {error, _} = E ->
            E;
        PID when is_pid(PID) ->
            vera_client:reload(PID),
            {ok, "Instructed the Vera to reload"}
    end.

alive(Name) ->
    case autopid(Name) of
        {error, _} = E ->
            E;
        PID when is_pid(PID) ->
            case vera_client:alive(PID) of
                ok ->
                    {ok, "Vera appears alive"}
            end
    end.

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
                IP when is_list(IP) ->
                    veraerl:start_child(IP, "auto"),
                    {ok, "Configured ~p as auto", [IP]}
            end
    end.
