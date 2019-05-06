%%%-------------------------------------------------------------------
%%% @author Marcin
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. May 2019 11:18 PM
%%%-------------------------------------------------------------------
-module(pollution_test).
-author("Marcin").

-include_lib("eunit/include/eunit.hrl").

-compile(exprots_all).

-record(monitor, {stations = #{}, measures = #{}}).
-record(station, {name, coordinates}).
-record(measurement, {value}).

createModule_test() ->
  ExpectedValue = #monitor{},
  Result = pollution:createMonitor(),
  ?assertEqual(ExpectedValue, Result).

addStationOK_test() ->
  ExpectedValue = {monitor, #{{55.5, 77.7} => {station, "Monte Carlo", {55.5, 77.7}},
    "Monte Carlo" => {station, "Monte Carlo", {55.5, 77.7}}},
    #{}},
  M = pollution:createMonitor(),
  M1 = pollution:addStation("Monte Carlo", {55.5, 77.7}, M),
  ?assertEqual(ExpectedValue, M1).

addStationBadArgumentError_test() ->
  ExpectedValue = "Bad arguments in function addStation(Name, {N,E}, Monitor).",
  M = pollution:createMonitor(),
  {_, Val1} = pollution:addStation(monte_carlo, {55.5, 77.7}, M),
  {_, Val2} = pollution:addStation("Monte Carlo", {55, 77.7}, M),
  {_, Val3} = pollution:addStation("Monte Carlo", {55.5, 77}, M),
  {_, Val4} = pollution:addStation(monte_carlo, {55, 77}, M),
  ?assertEqual(ExpectedValue, Val1),
  ?assertEqual(ExpectedValue, Val2),
  ?assertEqual(ExpectedValue, Val3),
  ?assertEqual(ExpectedValue, Val4).

addStationStationAlreadyExist_test() ->
  ExpectedValue = "Unable to addStation, station already exists.",
  M = pollution:createMonitor(),
  M1 = pollution:addStation("Monte Carlo", {55.5, 77.7}, M),
  {_, Val1} = pollution:addStation("Monte Carlo", {55.5, 77.7}, M1),
  ?assertEqual(ExpectedValue, Val1).

addValueOK_test() ->
  ExpectedValue = {monitor, #{{55.5, 77.7} => {station, "Khoroinis", {55.5, 77.7}},
    "Khoroinis" => {station, "Khoroinis", {55.5, 77.7}}},
    #{{"Khoroinis", {55.5, 77.7}, {{2019, 5, 1}, {23, 37, 45}}, "PM10"} =>
    {measurement, 125}}},
  M = pollution:createMonitor(),
  M1 = pollution:addStation("Khoroinis", {55.5, 77.7}, M),
  M2 = pollution:addValue("Khoroinis", {{2019, 5, 1}, {23, 37, 45}}, "PM10", 125, M1),
  ?assertEqual(ExpectedValue, M2).

addValueBadArgumentError_test() ->
  ExpectedValue = "Bad arguments in function addValue(Key, Date, Type, Value, Monitor).",
  M = pollution:createMonitor(),
  M1 = pollution:addStation("Vizima", {55.5, 77.7}, M),
  {_, Val1} = pollution:addValue("Vizima", {{2019, 5, 1}, {23, 37, 45}}, pm10, 125, M1),
  {_, Val2} = pollution:addValue("Vizima", {{2019, 5, 1}, {23, 37, 45}}, "PM10", test, M1),
  {_, Val3} = pollution:addValue("Vizima", {{2019, 5, 1}, {23, 37, 45}}, pm10, test, M1),
  ?assertEqual(ExpectedValue, Val1),
  ?assertEqual(ExpectedValue, Val2),
  ?assertEqual(ExpectedValue, Val3).

addValueStationDoesntExist_test() ->
  ExpectedVal = "Unable to add measurement, station doesn't exist.",
  Date = {{2019, 5, 1}, {23, 37, 45}},
  M = pollution:createMonitor(),
  {_, Val1} = pollution:addValue("Tretogor", Date, "PM10", 105, M),
  ?assertEqual(ExpectedVal, Val1).

addValueMeasurementAlreadyExist_test() ->
  ExpectedVal = "Unable to add measurement, measurement already exists.",
  Date = {{2019, 5, 1}, {23, 37, 45}},
  M = pollution:createMonitor(),
  M1 = pollution:addStation("Vengerberg", {55.5, 77.7}, M),
  M2 = pollution:addValue("Vengerberg", Date, "PM10", 105, M1),
  {_, Val1} = pollution:addValue("Vengerberg", Date, "PM10", 105, M2),
  ?assertEqual(ExpectedVal, Val1).

removeValueSingleOK_test() ->
  ExpectedValue = {monitor, #{{55.5, 77.7} => {station, "Aldersberg", {55.5, 77.7}},
    {55.6, 77.8} => {station, "Rivia", {55.6, 77.8}},
    "Aldersberg" => {station, "Aldersberg", {55.5, 77.7}},
    "Rivia" => {station, "Rivia", {55.6, 77.8}}},
    #{}},
  Date = {{2019, 5, 1}, {23, 37, 45}},
  M = pollution:createMonitor(),
  M1 = pollution:addStation("Aldersberg", {55.5, 77.7}, M),
  M2 = pollution:addStation("Rivia", {55.6, 77.8}, M1),
  M3 = pollution:addValue("Aldersberg", Date, "PM10", 105, M2),
  M4 = pollution:addValue({55.6, 77.8}, Date, "PM10", 105, M3),
  M5 = pollution:removeValue("Aldersberg", Date, "PM10", M4),
  M6 = pollution:removeValue({55.6, 77.8}, Date, "PM10", M5),
  ?assertEqual(ExpectedValue, M6).

removeValueBadArgumentError_test() ->
  ExpectedVal = "Bad arguments in function removeValue(Key, Date, Type, Monitor).",
  Date = {{2019, 5, 1}, {23, 37, 45}},
  M = pollution:createMonitor(),
  M1 = pollution:addStation("Scala", {55.5, 77.7}, M),
  M2 = pollution:addValue("Scala", Date, "PM10", 105, M1),
  {_, Val1} = pollution:removeValue("Scala", Date, test, M2),
  ?assertEqual(ExpectedVal, Val1).

removeValueStationDoesntExist_test() ->
  ExpectedVal = "Unable to remove measurement, station doesn't exist.",
  Date = {{2019, 5, 1}, {23, 37, 45}},
  M = pollution:createMonitor(),
  {_, Val1} = pollution:removeValue("Scala", Date, "PM10", M),
  ?assertEqual(ExpectedVal, Val1).

removeValueMeasurementDoesntExist_test() ->
  ExpectedVal = "Unable to remove measurement, measurement doesn't exist.",
  Date = {{2019, 5, 1}, {23, 37, 45}},
  M = pollution:createMonitor(),
  M1 = pollution:addStation("Vergen", {55.5, 77.7}, M),
  {_, Val1} = pollution:removeValue("Vergen", Date, "PM10", M1),
  ?assertEqual(ExpectedVal, Val1).

getOneValueOK_test() ->
  Date = {{2019, 5, 1}, {23, 37, 45}},
  M = pollution:createMonitor(),
  M1 = pollution:addStation("Rinbe", {55.5, 77.7}, M),
  M2 = pollution:addValue("Rinbe", Date, "PM10", 105, M1),
  Val1 = pollution:getOneValue("Rinbe", Date, "PM10", M2),
  ?assertEqual(105, Val1).

getOneValueBadArgumentError_test() ->
  ExpectedValue = "Bad arguments in function getOneValue(Key, Date, Type, Monitor).",
  Date = {{2019, 5, 1}, {23, 37, 45}},
  M = pollution:createMonitor(),
  M1 = pollution:addStation("Novigrad", {55.5, 77.7}, M),
  M2 = pollution:addValue("Novigrad", Date, "PM10", 105, M1),
  {_, Val1} = pollution:getOneValue("Novigrad", Date, pm10, M2),
  ?assertEqual(ExpectedValue, Val1).

getOneValueStationDoesntExist_test() ->
  ExpectedVal = "Unable to return measurement, station doesn't exist.",
  Date = {{2019, 5, 1}, {23, 37, 45}},
  M = pollution:createMonitor(),
  {_, Val1} = pollution:getOneValue("Oxenfurt", Date, "PM10", M),
  ?assertEqual(ExpectedVal, Val1).

getOneValueMeasurementDoesntExist_test() ->
  ExpectedVal = "Unable to return measurement, measurement doesn't exist.",
  Date = {{2019, 5, 1}, {23, 37, 45}},
  M = pollution:createMonitor(),
  M1 = pollution:addStation("Hagge", {55.5, 77.7}, M),
  {_, Val1} = pollution:getOneValue("Hagge", Date, "PM10", M1),
  ?assertEqual(ExpectedVal, Val1).

getStationMeanOK_test() ->
  M = pollution:createMonitor(),
  M1 = pollution:addStation("Ellander", {55.5, 77.7}, M),
  M2 = pollution:addValue("Ellander", {{2019, 5, 1}, {19, 11, 10}}, "PM10", 125, M1),
  M3 = pollution:addValue("Ellander", {{2019, 4, 1}, {19, 11, 10}}, "PM10", 127, M2),
  Val1 = pollution:getStationMean("Ellander", "PM10", M3),
  ?assertEqual(126.0, Val1).

getStationMeanBadArgumentError_test() ->
  ExpectedVal = "Bad arguments in function getStationMean(Key, Type, Monitor).",
  M = pollution:createMonitor(),
  M1 = pollution:addStation("Carreas", {55.5, 77.7}, M),
  M2 = pollution:addValue("Carreas", {{2019, 5, 1}, {19, 11, 10}}, "PM10", 125, M1),
  M3 = pollution:addValue("Carreas", {{2019, 4, 1}, {19, 11, 10}}, "PM10", 127, M2),
  {_, Val1} = pollution:getStationMean("Carreas", pm10, M3),
  ?assertEqual(ExpectedVal, Val1).

getStationMeanStationDoesntExistError_test() ->
  ExpectedVal = "Unable to calculate mean, station doesn't exist.",
  M = pollution:createMonitor(),
  M1 = pollution:addStation("Maribor", {55.5, 77.7}, M),
  M2 = pollution:addValue("Maribor", {{2019, 5, 1}, {19, 11, 10}}, "PM10", 125, M1),
  M3 = pollution:addValue("Maribor", {{2019, 4, 1}, {19, 11, 10}}, "PM10", 127, M2),
  {_, Val1} = pollution:getStationMean("Mayena", "PM10", M3),
  ?assertEqual(ExpectedVal, Val1).

getDailyMeanOK_test() ->
  M = pollution:createMonitor(),
  M1 = pollution:addStation("Dorian", {55.5, 77.7}, M),
  M2 = pollution:addValue("Dorian", {{2019, 5, 1}, {19, 11, 10}}, "PM10", 125, M1),
  M3 = pollution:addValue("Dorian", {{2019, 5, 1}, {17, 11, 10}}, "PM10", 127, M2),
  Val1 = pollution:getDailyMean({{2019, 5, 1}, {19, 11, 10}}, "PM10", M3),
  ?assertEqual(126.0, Val1).

getDailyMeanOKNoDate_test() ->
  M = pollution:createMonitor(),
  M1 = pollution:addStation("Dorian", {55.5, 77.7}, M),
  Val1 = pollution:getDailyMean({{2019, 5, 1}, {19, 11, 10}}, "PM10", M1),
  ?assertEqual(0.0, Val1).

getDailyMeanBadArgumentError_test() ->
  ExpectedVal = "Bad arguments in function getDailyMean(Date, Type, Monitor).",
  M = pollution:createMonitor(),
  M1 = pollution:addStation("Gors Vellen", {55.5, 77.7}, M),
  M2 = pollution:addValue("Gors Vellen", {{2019, 5, 1}, {19, 11, 10}}, "PM10", 125, M1),
  M3 = pollution:addValue("Gors Vellen", {{2019, 5, 1}, {17, 11, 10}}, "PM10", 127, M2),
  {_, Val1} = pollution:getDailyMean({{2019, 5, 1}, {19, 11, 10}}, pm10, M3),
  ?assertEqual(ExpectedVal, Val1).

getCorrelationOK_test() ->
  M = pollution:createMonitor(),
  M1 = pollution:addStation("Cidaris", {55.5, 77.7}, M),
  M2 = pollution:addValue("Cidaris", {{2019, 5, 1}, {19, 11, 10}}, "PM10", 125, M1),
  M3 = pollution:addValue("Cidaris", {{2019, 5, 1}, {19, 11, 10}}, "PM2,5", 127, M2),
  Val1 = pollution:getCorrelation("PM10", "PM2,5", M3),
  ?assertEqual(0.0, Val1).

getCorrelationBadArgumentError_test() ->
  ExpectedValue = "Bad arguments in function getCorrelation(Type1, Type2, Monitor).",
  M = pollution:createMonitor(),
  M1 = pollution:addStation("Kerack", {55.5, 77.7}, M),
  M2 = pollution:addValue("Kerack", {{2019, 5, 1}, {19, 11, 10}}, "PM10", 125, M1),
  M3 = pollution:addValue("Kerack", {{2019, 5, 1}, {19, 11, 10}}, "PM2,5", 127, M2),
  {_, Val1} = pollution:getCorrelation(pm10, "PM2,5", M3),
  {_, Val2} = pollution:getCorrelation("PM10", pm2, M3),
  {_, Val3} = pollution:getCorrelation(pm10, pm2, M3),
  ?assertEqual(ExpectedValue, Val1),
  ?assertEqual(ExpectedValue, Val2),
  ?assertEqual(ExpectedValue, Val3).

getCorrelationOneListEmptyError_test() ->
  ExpectedValue = "Unable to measure correlation, one list is empty",
  M = pollution:createMonitor(),
  M1 = pollution:addStation("Cintra", {55.5, 77.7}, M),
  M2 = pollution:addValue("Cintra", {{2019, 5, 1}, {19, 11, 10}}, "PM10", 125, M1),
  M3 = pollution:addValue("Cintra", {{2019, 5, 1}, {19, 11, 11}}, "PM10", 127, M2),
  {_, Val1} = pollution:getCorrelation("PM10", "PM2,5", M3),
  ?assertEqual(ExpectedValue, Val1).