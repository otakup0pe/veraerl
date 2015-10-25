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
    {ok, lists:map(fun({ID, DName}) ->
                          io_lib:format("#~4..0B ~p~n", [ID, binary_to_list(DName)])
                   end, vera_client:scene_list(veraerl:pid(Name)))}.

scene_run(Name, Scene) ->
    PID = veraerl:pid(Name),
    ok = vera_client:scene(PID, vera_client:scene_id(PID, Scene)),
    {ok, "Should have run scene"}.
