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
    {request, Pid, {addStation, {Name, {N, E}}}} ->
      io:write(Pid),
      M1 = pollution:addStation(Name, {N, E}, M),
      Pid ! {reply, ok},
      loop(M1);

    {request, Pid, {addValue, {Key, Date, Type, Value}}} ->
      M1 = pollution:addValue(Key, Date, Type, Value, M),
      Pid ! {reply, ok},
      loop(M1);

    {request, Pid, {removeValue, {Key, Date, Type}}} ->
      M1 = pollution:removeValue(Key, Date, Type, M),
      Pid ! {reply, ok},
      loop(M1);

    {request, Pid, {getOneValue, {Key, Date, Type}}} ->
      V = pollution:getOneValue(Key, Date, Type, M),
      Pid ! {reply, V},
      loop(M);

    {request, Pid, {getStationMean, {Key, Type}}} ->
      V = pollution:getStationMean(Key, Type, M),
      Pid ! {reply, V},
      loop(M);

    {request, Pid, {getDailyMean, {Date, Type}}} ->
      V = pollution:getDailyMean(Date, Type, M),
      Pid ! {reply, V},
      loop(M);

    {request, Pid, {getCorrelation, {Type1, Type2}}} ->
      V = pollution:getCorrelation(Type1, Type2, M),
      Pid ! {reply, V},
      loop(M)
  end.

stop() -> ok.

call(Message) ->
  p_server ! {request, self(), Message},
  receive
    {reply, Reply} -> Reply
  end.

addStation(Name, {N, E}) ->
  Message = {addStation, {Name, {N, E}}},
  call(Message).

addValue(Key, Date, Type, Value) ->
  Message = {addValue, {Key, Date, Type, Value}},
  call(Message).

removeValue(Key, Date, Type) ->
  Message = {removeValue, {Key, Date, Type}},
  call(Message).

getOneValue(Key, Date, Type) ->
  Message = {getOneValue, {Key, Date, Type}},
  call(Message).

getStationMean(Key, Type) ->
  Message = {getStationMean, {Key, Type}},
  call(Message).

getDailyMean(Date, Type) ->
  Message = {getDailyMean, {Date, Type}},
  call(Message).

getCorrelation(Type1, Type2) ->
  Message = {getCorrelation, {Type1, Type2}},
  call(Message).