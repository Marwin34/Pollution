%%%-------------------------------------------------------------------
%%% @author Marcin
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. May 2019 4:54 PM
%%%-------------------------------------------------------------------
-module(pollution_supervisor).
-behaviour(supervisor).
-author("Marcin").

%% API
-export([start/0, initialize_ets/0]).
-export([init/1]).

initialize_ets() ->
  Table = ets:new(monitors, [set, named_table, public]),
  ets:insert(Table, {monitor, pollution:createMonitor()}).

start() ->
  initialize_ets(),
  supervisor:start_link({local, pollution_server_supervisor}, pollution_supervisor,
    monitors).

init(InitialValue) ->
  {ok, {
    {one_for_all, 2, 3},
    [
      {pollution_server,
        {pollution_gen_server, start_link, [InitialValue]},
        permanent, brutal_kill, worker, [pollution_gen_server]}
    ]}
  }.