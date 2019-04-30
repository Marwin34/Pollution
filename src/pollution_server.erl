%%%-------------------------------------------------------------------
%%% @author Marcin
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Apr 2019 11:17 AM
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("Marcin").

%% API
-export([start/0, stop/0, init/0]).
-export([addStation/2, addValue/4, removeValue/3, getOneValue/3, getStationMean/2, getDailyMean/2,
  getCorrelation/2]).

start() ->
  register(p_server, spawn(pollution_server, init, [])).

init() ->
  loop(pollution:createMonitor()).

loop(M) ->
  receive
    {request, Pid, add_station, {Name, {N, E}}} ->
      M1 = pollution:addStation(Name, {N, E}, M),
      Pid ! {reply, ok},
      loop(M1);

    {request, Pid, add_value, {Key, Date, Type, Value}} ->
      M1 = pollution:addValue(Key, Date, Type, Value, M),
      Pid ! {reply, ok},
      loop(M1);

    {request, Pid, remove_value, {Key, Date, Type}} ->
      M1 = pollution:removeValue(Key, Date, Type, M),
      Pid ! {reply, ok},
      loop(M1);

    {request, Pid, get_one_value, {Key, Date, Type}} ->
      V = pollution:getOneValue(Key, Date, Type, M),
      Pid ! {reply, V},
      loop(M);

    {request, Pid, get_station_mean, {Key, Type}} ->
      V = pollution:getStationMean(Key, Type, M),
      Pid ! {reply, V},
      loop(M);

    {request, Pid, get_daily_mean, {Date, Type}} ->
      V = pollution:getDailyMean(Date, Type, M),
      Pid ! {reply, V},
      loop(M);

    {request, Pid, get_correlation, {Type1, Type2}} ->
      V = pollution:getCorrelation(Type1, Type2, M),
      Pid ! {reply, V},
      loop(M);

    {request, Pid, test} ->
      Pid ! {reply, test},
      loop(M)
  end.

stop() -> ok.

addStation(Name, {N, E}) ->
  p_server ! {request, self(), add_station, {Name, {N, E}}}.

addValue(Key, Date, Type, Value) ->
  p_server ! {request, self(), add_value, {Key, Date, Type, Value}}.

removeValue(Key, Date, Type) ->
  p_server ! {request, self(), remove_value, {Key, Date, Type}}.

getOneValue(Key, Date, Type) ->
  p_server ! {request, self(), get_one_value, {Key, Date, Type}}.

getStationMean(Key, Type) ->
  p_server ! {request, self(), get_station_mean, {Key, Type}}.

getDailyMean(Date, Type) ->
  p_server ! {request, self(), get_daily_mean, {Date, Type}}.

getCorrelation(Type1, Type2) ->
  p_server ! {request, self(), get_correlation, {Type1, Type2}}.
