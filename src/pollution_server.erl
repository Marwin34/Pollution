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
  getCorrelation/2, crash/0]).

start() ->
  register(p_server, spawn(pollution_server, init, [])).

init() ->
  loop(pollution:createMonitor()).

loop(M) ->
  receive
    {request, Pid, {addStation, {Name, {N, E}}}} ->
      Result = {monitor, pollution:addStation(Name, {N, E}, M)},
      fetchResult(Result, Pid, M);

    {request, Pid, {addValue, {Key, Date, Type, Value}}} ->
      Result = {monitor, pollution:addValue(Key, Date, Type, Value, M)},
      fetchResult(Result, Pid, M);

    {request, Pid, {removeValue, {Key, Date, Type}}} ->
      Result = {monitor, pollution:removeValue(Key, Date, Type, M)},
      fetchResult(Result, Pid, M);

    {request, Pid, {getOneValue, {Key, Date, Type}}} ->
      Result = {value, pollution:getOneValue(Key, Date, Type, M)},
      fetchResult(Result, Pid, M);

    {request, Pid, {getStationMean, {Key, Type}}} ->
      Result = {value, pollution:getStationMean(Key, Type, M)},
      fetchResult(Result, Pid, M);

    {request, Pid, {getDailyMean, {Date, Type}}} ->
      Result = {value, pollution:getDailyMean(Date, Type, M)},
      fetchResult(Result, Pid, M);

    {request, Pid, {getCorrelation, {Type1, Type2}}} ->
      Result = {value, pollution:getCorrelation(Type1, Type2, M)},
      fetchResult(Result, Pid, M);

    {request, Pid, stop} ->
      Pid ! {stop, ok};

    {request, Pid, debug} ->
      Pid ! {debug, M};

    {request, crash} ->
      1/0
  end.

call(Message) ->
  case whereis(p_server) of
    undefined -> {error, "Pollution server undefined"};
    _ -> p_server ! {request, self(), Message},
      receive
        {_, Reply} -> Reply
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

crash() ->
  p_server ! {request, crash}.

fetchResult(Result, Pid, M) ->
  case Result of
    {monitor, Value} ->
      case Value of
        {error, Message} -> Pid ! {error, Message}, loop(M);
        _ -> Pid ! {monitor, ok}, loop(Value)
      end;
    {value, Value} ->
      case Value of
        {error, Message} -> Pid ! {error, Message}, loop(M);
        _ -> Pid ! {value, Value}, loop(M)
      end
  end.