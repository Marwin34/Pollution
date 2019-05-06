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
      M2 = pollution:addStation("Sacz", {55.5, 77.7}, M1),
      ?assertEqual(M2, M)
  after
    10 ->
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
      M2 = pollution:addStation("Sacz", {55.5, 77.7}, M1),
      M3 = pollution:addStation("Sacz Stary", {45.6, 65.4}, M2),
      ?assertEqual(M3, M)
  after
    10 ->
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
      M2 = pollution:addStation("Sacz", {55.5, 77.7}, M1),
      M3 = pollution:addStation("Stary Sacz", {55.6, 77.8}, M2),
      M4 = pollution:addValue("Sacz", Date, "PM10", 105, M3),
      M5 = pollution:addValue({55.6, 77.8}, Date, "PM10", 105, M4),
      ?assertEqual(M5, M)
  after
    10 ->
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
      M2 = pollution:addStation("Sacz", {55.5, 77.7}, M1),
      M3 = pollution:addValue("Sacz", Date, "PM10", 105, M2),
      M4 = pollution:addValue("Sacz", Date, "PM2,5", 127, M3),
      ?assertEqual(M4, M)
  after
    10 ->
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
      M2 = pollution:addStation("Sacz", {55.5, 77.7}, M1),
      M3 = pollution:addStation("Stary Sacz", {55.6, 77.8}, M2),
      ?assertEqual(M3, M)
  after
    10 ->
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
      M2 = pollution:addStation("Sacz", {55.5, 77.7}, M1),
      ?assertEqual(M2, M)
  after
    10 ->
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
  Val1 = pollution_server:getOneValue("Sacz", Date, "PM10"),
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

getOneValueStationDoesntExist_test() ->
  ExpectedVal = "Unable to return measurement, station doesn't exist.",
  Date = calendar:local_time(),
  pollution_server:start(),
  Val1 = pollution_server:getOneValue("Sacz", Date, "PM10"),
  pollution_server:stop(),
  ?assertEqual(ExpectedVal, Val1).

getOneValueMeasurementDoesntExist_test() ->
  ExpectedVal = "Unable to return measurement, measurement doesn't exist.",
  Date = calendar:local_time(),
  pollution_server:start(),
  pollution_server:addStation("Sacz", {55.5, 77.7}),
  Val1 = pollution_server:getOneValue("Sacz", Date, "PM10"),
  pollution_server:stop(),
  ?assertEqual(ExpectedVal, Val1).

getStationMeanOK_test() ->
  pollution_server:start(),
  pollution_server:addStation("Sacz", {55.5, 77.7}),
  pollution_server:addValue("Sacz", {{2019,5,1},{19,11,10}}, "PM10", 125),
  pollution_server:addValue("Sacz", {{2019,4,1},{19,11,10}}, "PM10", 127),
  Val1 = pollution_server:getStationMean("Sacz", "PM10"),
  pollution_server:stop(),
  ?assertEqual(126.0, Val1).

getStationMeanBadArgumentError_test() ->
  ExpectedVal =  "Bad arguments in function getStationMean(Key, Type, Monitor).",
  pollution_server:start(),
  pollution_server:addStation("Sacz", {55.5, 77.7}),
  pollution_server:addValue("Sacz", {{2019,5,1},{19,11,10}}, "PM10", 125),
  pollution_server:addValue("Sacz", {{2019,4,1},{19,11,10}}, "PM10", 127),
  Val1 = pollution_server:getStationMean("Sacz", pm10),
  pollution_server:stop(),
  ?assertEqual(ExpectedVal, Val1).

getStationMeanStationDoesntExistError_test() ->
  ExpectedVal =  "Unable to calculate mean, station doesn't exist.",
  pollution_server:start(),
  pollution_server:addStation("Sacz", {55.5, 77.7}),
  pollution_server:addValue("Sacz", {{2019,5,1},{19,11,10}}, "PM10", 125),
  pollution_server:addValue("Sacz", {{2019,4,1},{19,11,10}}, "PM10", 127),
  Val1 = pollution_server:getStationMean(sacz, "PM10"),
  pollution_server:stop(),
  ?assertEqual(ExpectedVal, Val1).

getDailyMeanOK_test() ->
  pollution_server:start(),
  pollution_server:addStation("Sacz", {55.5, 77.7}),
  pollution_server:addValue("Sacz", {{2019,5,1},{19,11,10}}, "PM10", 125),
  pollution_server:addValue("Sacz", {{2019,5,1},{17,11,10}}, "PM10", 127),
  Val1 = pollution_server:getDailyMean({{2019,5,1},{19,11,10}}, "PM10"),
  pollution_server:stop(),
  ?assertEqual(126.0, Val1).

getDailyMeanOKNoDate_test() ->
  pollution_server:start(),
  pollution_server:addStation("Sacz", {55.5, 77.7}),
  Val1 = pollution_server:getDailyMean({{2019,5,1},{19,11,10}}, "PM10"),
  pollution_server:stop(),
  ?assertEqual(0.0, Val1).

getDailyMeanBadArgumentError_test() ->
  ExpectedVal = "Bad arguments in function getDailyMean(Date, Type, Monitor).",
  pollution_server:start(),
  pollution_server:addStation("Sacz", {55.5, 77.7}),
  pollution_server:addValue("Sacz", {{2019,5,1},{19,11,10}}, "PM10", 125),
  pollution_server:addValue("Sacz", {{2019,5,1},{17,11,10}}, "PM10", 127),
  Val1 = pollution_server:getDailyMean({{2019,5,1},{19,11,10}}, pm10),
  pollution_server:stop(),
  ?assertEqual(ExpectedVal, Val1).

getCorrelationOK_test() ->
  pollution_server:start(),
  pollution_server:addStation("Sacz", {55.5, 77.7}),
  pollution_server:addStation("Krynica", {12.3, 76.0}),
  pollution_server:addStation("Grudek", {32.1, 34.0}),
  pollution_server:addValue("Sacz", {{2019,5,1},{19,11,10}}, "PM10", 125),
  pollution_server:addValue("Sacz", {{2019,5,1},{19,11,10}}, "PM2,5", 127),
  pollution_server:addValue("Krynica", {{2019,5,1},{19,11,10}}, "PM10", 65),
  pollution_server:addValue("Krynica", {{2019,5,1},{19,11,10}}, "PM2,5", 45),
  pollution_server:addValue("Grudek", {{2019,5,1},{19,11,10}}, "PM10", 198),
  pollution_server:addValue("Grudek", {{2019,5,1},{19,11,10}}, "PM2,5", 171),
  Val1 = pollution_server:getCorrelation("PM10", "PM2,5"),
  pollution_server:stop(),
  ?assertEqual(13411, round(Val1 * 1000)).

getCorrelationBadArgumentError_test() ->
  ExpectedValue = "Bad arguments in function getCorrelation(Type1, Type2, Monitor).",
  pollution_server:start(),
  pollution_server:addStation("Sacz", {55.5, 77.7}),
  pollution_server:addValue("Sacz", {{2019,5,1},{19,11,10}}, "PM10", 125),
  pollution_server:addValue("Sacz", {{2019,5,1},{19,11,10}}, "PM2,5", 127),
  Val1 = pollution_server:getCorrelation(pm10, "PM2,5"),
  Val2 = pollution_server:getCorrelation("PM10", pm2),
  Val3 = pollution_server:getCorrelation(pm10, pm2),
  pollution_server:stop(),
  ?assertEqual(ExpectedValue, Val1),
  ?assertEqual(ExpectedValue, Val2),
  ?assertEqual(ExpectedValue, Val3).

getCorrelationOneListEmptyError_test() ->
  ExpectedValue = "Unable to measure correlation, one list is empty",
  pollution_server:start(),
  pollution_server:addStation("Sacz", {55.5, 77.7}),
  pollution_server:addValue("Sacz", {{2019,5,1},{19,11,10}}, "PM10", 125),
  pollution_server:addValue("Sacz", {{2019,5,1},{19,11,11}}, "PM10", 127),
  Val1 = pollution_server:getCorrelation("PM10", "PM2,5"),
  pollution_server:stop(),
  ?assertEqual(ExpectedValue, Val1).