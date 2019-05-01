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
      fetchResult(pollution:addStation(Name, {N, E}, M), Pid, M);

    {request, Pid, {addValue, {Key, Date, Type, Value}}} ->
      fetchResult(pollution:addValue(Key, Date, Type, Value, M), Pid, M);

    {request, Pid, {removeValue, {Key, Date, Type}}} ->
      fetchResult(pollution:removeValue(Key, Date, Type, M), Pid, M);

    {request, Pid, {getOneValue, {Key, Date, Type}}} ->
      fetchResult(pollution:getOneValue(Key, Date, Type, M), Pid, M);

    {request, Pid, {getStationMean, {Key, Type}}} ->
      fetchResult(pollution:getStationMean(Key, Type, M), Pid, M);

    {request, Pid, {getDailyMean, {Date, Type}}} ->
      fetchResult(pollution:getDailyMean(Date, Type, M), Pid, M);

    {request, Pid, {getCorrelation, {Type1, Type2}}} ->
      fetchResult(pollution:getCorrelation(Type1, Type2, M), Pid, M);

    {request, Pid, stop} ->
      Pid ! {reply, ok}
  end.

call(Message) ->
  case whereis(p_server) of
    undefined -> {error, "Pollution server undefined"};
    _ -> p_server ! {request, self(), Message},
      receive
        {reply, Reply} -> Reply;
        {error, Reply} -> Reply
      end
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

stop() ->
  call(stop).

fetchResult(Result, Pid, M) ->
  case Result of
    {error, Message} ->
      Pid ! {error, Message},
      loop(M);
    M1 ->
      Pid ! {reply, ok},
      loop(M1)
  end.