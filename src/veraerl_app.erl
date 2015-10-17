-module(veraerl_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    {ok, PID} = veraerl_sup:start_link(master),
    {ok, PID, {}}.

stop(_State) ->
    ok.
