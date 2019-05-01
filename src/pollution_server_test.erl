%%%-------------------------------------------------------------------
%%% @author Marcin
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. May 2019 3:25 PM
%%%-------------------------------------------------------------------
-module(pollution_server_test).
-author("Marcin").

-include_lib("eunit/include/eunit.hrl").

-compile(exprots_all).

start_test() ->
  pollution_server:start(),
  ?assertNotEqual(undefined, whereis(p_server)).

stop_test() ->
  pollution_server:stop(),
  ?assertEqual(undefined, whereis(p_server)).

addStationSingleOK_test() ->
  pollution_server:start(),
  pollution_server:addStation("Sacz", {55.5, 77.7}),
  p_server ! {request, self(), debug},
  receive
    {debug, M} ->
      pollution_server:stop(),
      M1 = pollution:createMonitor(),
      {_, M2} = pollution:addStation("Sacz", {55.5, 77.7}, M1),
      ?assertEqual(M2, M)
  after
    1000 ->
      pollution_server:stop(),
      ?assert(false)
  end.

addStationMultipleOK_test() ->
  pollution_server:start(),
  pollution_server:addStation("Sacz", {55.5, 77.7}),
  pollution_server:addStation("Sacz Stary", {45.6, 65.4}),
  p_server ! {request, self(), debug},
  receive
    {debug, M} ->
      pollution_server:stop(),
      M1 = pollution:createMonitor(),
      {_, M2} = pollution:addStation("Sacz", {55.5, 77.7}, M1),
      {_, M3} = pollution:addStation("Sacz Stary", {45.6, 65.4}, M2),
      ?assertEqual(M3, M)
  after
    1000 ->
      pollution_server:stop(),
      ?assert(false)
  end.

addStationBadArgumentError_test() ->
  pollution_server:start(),
  ExpectedVal = "Bad arguments in function addStation(Name, {N,E}, Monitor).",
  Val1 = pollution_server:addStation(sacz, {55.5, 77.7}),
  Val2 = pollution_server:addStation("Sacz", {55.5, 77}),
  Val3 = pollution_server:addStation("Sacz", {55, 77.7}),
  Val4 = pollution_server:addStation("Sacz", {55, 77}),
  Val5 = pollution_server:addStation(sazc, {55, 77}),
  pollution_server:stop(),
  ?assertEqual(ExpectedVal, Val1),
  ?assertEqual(ExpectedVal, Val2),
  ?assertEqual(ExpectedVal, Val3),
  ?assertEqual(ExpectedVal, Val4),
  ?assertEqual(ExpectedVal, Val5).

addStationAlreadyExistError_test() ->
  pollution_server:start(),
  ExpectedVal = "Unable to addStation, station already exists.",
  pollution_server:addStation("Sacz", {55.5, 77.7}),
  Value = pollution_server:addStation("Sacz", {55.5, 77.7}),
  pollution_server:stop(),
  ?assertEqual(ExpectedVal, Value).

addValueSingleOK_test() ->
  pollution_server:start(),
  pollution_server:addStation("Sacz", {55.5, 77.7}),
  pollution_server:addStation("Stary Sacz", {55.6, 77.8}),
  Date = calendar:local_time(),
  pollution_server:addValue("Sacz", Date, "PM10", 105),
  pollution_server:addValue({55.6, 77.8}, Date, "PM10", 105),
  p_server ! {request, self(), debug},
  receive
    {debug, M} ->
      pollution_server:stop(),
      M1 = pollution:createMonitor(),
      {_, M2} = pollution:addStation("Sacz", {55.5, 77.7}, M1),
      {_, M3} = pollution:addStation("Stary Sacz", {55.6, 77.8}, M2),
      {_, M4} = pollution:addValue("Sacz", Date, "PM10", 105, M3),
      {_, M5} = pollution:addValue({55.6, 77.8}, Date, "PM10", 105, M4),
      ?assertEqual(M5, M)
  after
    1000 ->
      pollution_server:stop(),
      ?assert(false)
  end.

addValueMultipleOK_test() ->
  pollution_server:start(),
  pollution_server:addStation("Sacz", {55.5, 77.7}),
  Date = calendar:local_time(),
  pollution_server:addValue("Sacz", Date, "PM10", 105),
  pollution_server:addValue("Sacz", Date, "PM2,5", 127),
  p_server ! {request, self(), debug},
  receive
    {debug, M} ->
      pollution_server:stop(),
      M1 = pollution:createMonitor(),
      {_, M2} = pollution:addStation("Sacz", {55.5, 77.7}, M1),
      {_, M3} = pollution:addValue("Sacz", Date, "PM10", 105, M2),
      {_, M4} = pollution:addValue("Sacz", Date, "PM2,5", 127, M3),
      ?assertEqual(M4, M)
  after
    1000 ->
      pollution_server:stop(),
      ?assert(false)
  end.

addValueBadArgumentError_test() ->
  ExpectedVal = "Bad arguments in function addValue(Key, Date, Type, Value, Monitor).",
  pollution_server:start(),
  pollution_server:addStation("Sacz", {55.5, 77.7}),
  Date = calendar:local_time(),
  Val1 = pollution_server:addValue("Sacz", Date, pm10, 105),
  Val2 = pollution_server:addValue("Sacz", Date, "PM10", test),
  Val3 = pollution_server:addValue("Sacz", Date, pm10, test),
  pollution_server:stop(),
  ?assertEqual(ExpectedVal, Val1),
  ?assertEqual(ExpectedVal, Val2),
  ?assertEqual(ExpectedVal, Val3).

addValueStationDoesntExist_test() ->
  ExpectedVal = "Unable to add measurement, station doesn't exist.",
  Date = calendar:local_time(),
  pollution_server:start(),
  Val1 = pollution_server:addValue("Sacz", Date, "PM10", 105),
  pollution_server:stop(),
  ?assertEqual(ExpectedVal, Val1).

addValueMeasurementAlreadyExist_test() ->
  ExpectedVal = "Unable to add measurement, measurement already exists.",
  Date = calendar:local_time(),
  pollution_server:start(),
  pollution_server:addStation("Sacz", {55.5, 77.7}),
  pollution_server:addValue("Sacz", Date, "PM10", 105),
  Val1 = pollution_server:addValue("Sacz", Date, "PM10", 105),
  pollution_server:stop(),
  ?assertEqual(ExpectedVal, Val1).

removeValueSingleOK_test() ->
  pollution_server:start(),
  pollution_server:addStation("Sacz", {55.5, 77.7}),
  pollution_server:addStation("Stary Sacz", {55.6, 77.8}),
  Date = calendar:local_time(),
  pollution_server:addValue("Sacz", Date, "PM10", 105),
  pollution_server:addValue({55.6, 77.8}, Date, "PM10", 105),
  pollution_server:removeValue("Sacz", Date, "PM10"),
  pollution_server:removeValue({55.6, 77.8}, Date, "PM10"),
  p_server ! {request, self(), debug},
  receive
    {debug, M} ->
      pollution_server:stop(),
      M1 = pollution:createMonitor(),
      {_, M2} = pollution:addStation("Sacz", {55.5, 77.7}, M1),
      {_, M3} = pollution:addStation("Stary Sacz", {55.6, 77.8}, M2),
      ?assertEqual(M3, M)
  after
    1000 ->
      pollution_server:stop(),
      ?assert(false)
  end.

removeValueMultipleOK_test() ->
  pollution_server:start(),
  pollution_server:addStation("Sacz", {55.5, 77.7}),
  Date = calendar:local_time(),
  pollution_server:addValue("Sacz", Date, "PM10", 105),
  pollution_server:addValue("Sacz", Date, "PM2,5", 127),
  pollution_server:removeValue("Sacz", Date, "PM10"),
  pollution_server:removeValue("Sacz", Date, "PM2,5"),
  p_server ! {request, self(), debug},
  receive
    {debug, M} ->
      pollution_server:stop(),
      M1 = pollution:createMonitor(),
      {_, M2} = pollution:addStation("Sacz", {55.5, 77.7}, M1),
      ?assertEqual(M2, M)
  after
    1000 ->
      pollution_server:stop(),
      ?assert(false)
  end.

removeValueBadArgumentError_test() ->
  ExpectedVal = "Bad arguments in function removeValue(Key, Date, Type, Monitor).",
  pollution_server:start(),
  pollution_server:addStation("Sacz", {55.5, 77.7}),
  Date = calendar:local_time(),
  pollution_server:addValue("Sacz", Date, "PM10", 105),
  Val1 = pollution_server:removeValue("Sacz", Date, test),
  pollution_server:stop(),
  ?assertEqual(ExpectedVal, Val1).

removeValueStationDoesntExist_test() ->
  ExpectedVal = "Unable to remove measurement, station doesn't exist.",
  Date = calendar:local_time(),
  pollution_server:start(),
  Val1 = pollution_server:removeValue("Sacz", Date, "PM10"),
  pollution_server:stop(),
  ?assertEqual(ExpectedVal, Val1).

removeValueMeasurementDoesntExist_test() ->
  ExpectedVal = "Unable to remove measurement, measurement doesn't exist.",
  Date = calendar:local_time(),
  pollution_server:start(),
  pollution_server:addStation("Sacz", {55.5, 77.7}),
  Val1 = pollution_server:removeValue("Sacz", Date, "PM10"),
  pollution_server:stop(),
  ?assertEqual(ExpectedVal, Val1).

getOneValueOK_test() ->
  pollution_server:start(),
  Date = calendar:local_time(),
  pollution_server:addStation("Sacz", {55.5, 77.7}),
  pollution_server:addValue("Sacz", Date, "PM10", 105),
  {_, Val1} = pollution_server:getOneValue("Sacz", Date, "PM10"),
  pollution_server:stop(),
  ?assertEqual(105, Val1).

getOneValueBadArgumentError_test() ->
  ExpectedValue = "Bad arguments in function getOneValue(Key, Date, Type, Monitor).",
  Date = calendar:local_time(),
  pollution_server:start(),
  pollution_server:addStation("Sacz", {55.5, 77.7}),
  pollution_server:addValue("Sacz", Date, "PM10", 105),
  Val1 = pollution_server:getOneValue("Sacz", Date, pm10),
  pollution_server:stop(),
  ?assertEqual(ExpectedValue, Val1).