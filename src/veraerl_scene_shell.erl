-module(veraerl_scene_shell).
-behaviour(shellbeam).

-export([commands/0]).
-export([scene_list/1, scene_run/2]).

commands() ->
    [
     {["list", {"vera", string}], "Scene list", fun scene_list/1},
     {["run",  {"vera", string}, {"scene", string}], "Run a scene", fun scene_run/2}
    ].

scene_list(Name) ->
    case veraerl_shell:autopid(Name) of
        {error, _} = E ->
            E;
        PID when is_pid(PID) ->
            {ok, lists:map(fun({ID, DName}) ->
                                   io_lib:format("#~4..0B ~p~n", [ID, binary_to_list(DName)])
                           end, vera_client:scene_list(PID))}
    end.

scene_run(Name, Scene) ->
    case veraerl_shell:autopid(Name) of
        {error, _} = E ->
            E;
        PID when is_pid(PID) ->
            ok = vera_client:scene(PID, vera_client:scene_id(PID, Scene)),
            {ok, "Instructed the Vera to run a scene"}
    end.
