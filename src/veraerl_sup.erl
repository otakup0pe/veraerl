-module(veraerl_sup).
-behaviour(supervisor).
-export([start_link/1, init/1]).

start_link(master) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, master);
start_link(child) ->
    supervisor:start_link({local, veraerl_child_sup}, ?MODULE, child).

init(master) ->
    {ok, {{rest_for_one, 1, 10},
          [
           {veraerl, {veraerl, start_link, []}, permanent, 5000, worker, [veraerl]},
           {vera_child_sup, {veraerl_sup, start_link, [child]}, permanent, infinity, supervisor, [vera_sup]}
          ]}};
init(child) ->
    {ok, {{simple_one_for_one, 10, 60},
          [
           {vera_client, {vera_client, start_link, []}, temporary, 5000, worker, [vera_client]}
          ]}}.
