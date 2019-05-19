%%%-------------------------------------------------------------------
%%% @author Marcin
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. May 2019 12:25 PM
%%%-------------------------------------------------------------------
-module(pollution_gen_server).
-behaviour(gen_server).

-author("Marcin").

%% API
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).
-export([start/0, addStation/2, addValue/4, removeValue/3, getOneValue/3,
  getStationMean/2, getDailyMean/2, getCorrelation/2, stop/0, crash/0]).
-export([test/0]).
-export([start_link/1]).

%% START %%
start() ->
  gen_server:start_link({local, pollution_server}, pollution_gen_server, pollution:createMonitor(), []).

%% SUPERVISOR START METHOD %%
start_link(InitialValue) ->
  gen_server:start_link({local, pollution_server}, pollution_gen_server, InitialValue, []).

%% USER INTERFACE %%
addStation(Name, {Latitude, Longitude}) ->
  gen_server:call(pollution_server, {addStation, Name, {Latitude, Longitude}}).

addValue(Key, Date, Type, Value) ->
  gen_server:call(pollution_server, {addValue, Key, Date, Type, Value}).

removeValue(Key, Date, Type) ->
  gen_server:call(pollution_server, {removeValue, Key, Date, Type}).

getOneValue(Key, Date, Type) ->
  gen_server:call(pollution_server, {getOneValue, Key, Date, Type}).

getStationMean(Key, Type) ->
  gen_server:call(pollution_server, {getStationMean, Key, Type}).

getDailyMean(Date, Type) ->
  gen_server:call(pollution_server, {getDailyMean, Date, Type}).

getCorrelation(Type1, Type2) ->
  gen_server:call(pollution_server, {getCorrelation, Type1, Type2}).

init(TableName) ->
  case ets:file2tab(TableName) of
    {error, Reason} -> io:format("Unable to open file:  ~p~n", [Reason]);
    {ok, Tab} ->
      [{monitor, InitialValue}] = ets:lookup(Tab, monitor),
      {ok, InitialValue}
  end.

stop() ->
  gen_server:cast(pollution_server, stop).

crash() ->
  gen_server:cast(pollution_server, crash).

%% CALLBACKS %%
handle_call(Message, _From, State) ->
  case ets:file2tab("test.txt") of
    {error, Reason} -> io:format("Unable to open file:  ~p~n", [Reason]);
    {ok, Tab} -> ets:insert(Tab, {monitor, State}), ets:tab2file(Tab, "test.txt")
  end,
  case Message of
    {addStation, Name, Coordinates} -> Result = {monitor, pollution:addStation(Name, Coordinates, State)};
    {addValue, Key, Date, Type, Value} -> Result = {monitor, pollution:addValue(Key, Date, Type, Value, State)};
    {removeValue, Key, Date, Type} -> Result = {monitor, pollution:removeValue(Key, Date, Type, State)};
    {getOneValue, Key, Date, Type} -> Result = {value, pollution:getOneValue(Key, Date, Type, State)};
    {getStationMean, Key, Type} -> Result = {value, pollution:getStationMean(Key, Type, State)};
    {getDailyMean, Date, Type} -> Result = {value, pollution:getDailyMean(Date, Type, State)};
    {getCorrelation, Type1, Type2} -> Result = {value, pollution:getCorrelation(Type1, Type2, State)}
  end,
  fetchResult(Result, State).

handle_cast(Request, State) ->
  case Request of
    stop -> {stop, normal, State};
    crash -> 1 / 0
  end.

terminate(Reason, Value) ->
  io:format("Server: exit with value ~p~n", [Value]),
  Reason.

fetchResult(Result, State) ->
  case Result of
    {monitor, Value} ->
      case Value of
        {error, Message} -> {reply, Message, State};
        _ -> {reply, ok, Value}
      end;
    {value, Value} ->
      case Value of
        {error, Message} -> {reply, Message, State};
        _ -> {reply, Value, State}
      end
  end.


%% OBSOLETE AFTER SUPERVISOR %%
test() ->
  start(),
  Value1 = addStation("Ionia", {45.6, 78.9}),
  io:format("Response: ~p~n", [Value1]),
  Value2 = addValue("Ionia", {{2019, 5, 17}, {15, 51, 4}}, "PM10", 125.0),
  io:format("Response: ~p~n", [Value2]),
  Value3 = getOneValue("Ionia", {{2019, 5, 17}, {15, 51, 4}}, "PM10"),
  io:format("Response: ~p~n", [Value3]),
  Value4 = getStationMean("Ionia", "PM10"),
  io:format("Response: ~p~n", [Value4]),
  Value5 = getDailyMean({{2019, 5, 17}, {15, 51, 4}}, "PM10"),
  io:format("Response: ~p~n", [Value5]),
  Value6 = getCorrelation("PM10", "PM2,5"),
  io:format("Response: ~p~n", [Value6]),
  Value7 = removeValue("Ionia", {{2019, 5, 17}, {15, 51, 4}}, "PM10"),
  io:format("Response: ~p~n", [Value7]),
  Value8 = stop(),
  io:format("Response: ~p~n", [Value8]).