%%%-------------------------------------------------------------------
%%% @author Marcin
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Apr 2019 10:47 AM
%%%-------------------------------------------------------------------
-module(pollution).
-author("Marcin").

-record(monitor, {stations = #{}, measures = #{}}).
-record(station, {name, coordinates}).
-record(measurement, {value}).

%% API
-export([createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/3,
  getCorrelation/3]).

createMonitor() ->
  #monitor{}.

addStation(Name, {N, E}, Monitor) ->
  Guard = validateArgument(Monitor, monitor_record) and validateArgument(Name, string_value) and
    validateArgument(N, float_value) and validateArgument(E, float_value),
  case Guard of
    true -> case maps:is_key(Name, Monitor#monitor.stations) or maps:is_key({N, E}, Monitor#monitor.stations) of
              true -> {error, "Unable to addStation, station already exists."};
              false ->
                Station = #station{name = Name, coordinates = {N, E}},
                Stations = (Monitor#monitor.stations)#{Name => Station, {N, E} => Station},
                {monitor, #monitor{stations = Stations, measures = Monitor#monitor.measures}}
            end;
    _ -> {error, "Bad arguments in function addStation(Name, {N,E}, Monitor)."}
  end.

addValue(Key, Date, Type, Value, Monitor) ->
  Guard = validateArgument(Type, string_value) and (validateArgument(Value, float_value) or validateArgument(Value, integer))
    and validateArgument(Monitor, monitor_record),
  case Guard of
    true -> case maps:is_key(Key, Monitor#monitor.stations) of
              true ->
                Stations = Monitor#monitor.stations,
                Station = maps:get(Key, Stations),
                StationName = Station#station.name,
                StationCoordinates = Station#station.coordinates,
                case maps:is_key({StationName, StationCoordinates, Date, Type}, Monitor#monitor.measures) of
                  true -> {error, "Unable to add measurement, measurement already exists."};
                  false ->
                    NewMeasures = (Monitor#monitor.measures)#{{StationName, StationCoordinates, Date, Type} =>
                    #measurement{value = Value}},
                    {monitor, #monitor{stations = Stations, measures = NewMeasures}}
                end;
              false -> {error, "Unable to add measurement, station doesn't exist."}
            end;
    _ -> {error, "Bad arguments in function addValue(Key, Date, Type, Value, Monitor)."}
  end.

removeValue(Key, Date, Type, Monitor) ->
  Guard = validateArgument(Type, string_value) and validateArgument(Monitor, monitor_record),
  case Guard of
    true -> case maps:is_key(Key, Monitor#monitor.stations) of
              true ->
                Stations = Monitor#monitor.stations,
                Station = maps:get(Key, Stations),
                StationName = Station#station.name,
                StationCoordinates = Station#station.coordinates,
                case maps:is_key({StationName, StationCoordinates, Date, Type}, Monitor#monitor.measures) of
                  true ->
                    NewMeasures = maps:remove({StationName, StationCoordinates, Date, Type}, Monitor#monitor.measures),
                    {monitor, #monitor{stations = Stations, measures = NewMeasures}};
                  false ->
                    {error, "Unable to remove measurement, measurement doesn't exist."}
                end;
              false -> {error, "Unable to remove measurement, station doesn't exist."}
            end;
    _ -> {error, "Bad arguments in function removeValue(Key, Date, Type, Monitor)."}
  end.

getOneValue(Key, Date, Type, Monitor) ->
  Guard = validateArgument(Type, string_value) and validateArgument(Monitor, monitor_record),
  case Guard of
    true -> case maps:is_key(Key, Monitor#monitor.stations) of
              true ->
                Stations = Monitor#monitor.stations,
                Station = maps:get(Key, Stations),
                StationName = Station#station.name,
                StationCoordinates = Station#station.coordinates,
                case maps:is_key({StationName, StationCoordinates, Date, Type}, Monitor#monitor.measures) of
                  true ->
                    Val = maps:get({StationName, StationCoordinates, Date, Type}, Monitor#monitor.measures),
                    ValNormalized = Val#measurement.value,
                    {value, ValNormalized};
                  false -> {error, "Unable to return measurement, measurement doesn't exist."}
                end;
              false -> {error, "Unable to return measurement, station doesn't exist."}
            end;
    _ -> {error, "Bad arguments in function getOneValue(Key, Date, Type, Monitor)."}
  end.

getStationMean(Key, Type, Monitor) ->
  Guard = validateArgument(Type, string_value) and validateArgument(Monitor, monitor_record),
  case Guard of
    true -> case maps:is_key(Key, Monitor#monitor.stations) of
              true ->
                Stations = Monitor#monitor.stations,
                Station = maps:get(Key, Stations),
                StationName = Station#station.name,
                StationCoordinates = Station#station.coordinates,
                List = maps:to_list(maps:filter(fun({StationName2, StationCoordinates2, _, Type2}, _) ->
                  (StationName2 == StationName) and (StationCoordinates2 == StationCoordinates) and (Type2 == Type) end,
                  Monitor#monitor.measures)),
                {value, calculateAvg(List, 0, 0)};
              false -> {error, "Unable to calculate mean, station doesn't exist."}
            end;
    _ -> {error, "Bad arguments in function getStationMean(Key, Type, Monitor)."}
  end.

getDailyMean(Date, Type, Monitor) ->
  Guard = validateArgument(Type, string_value) and validateArgument(Monitor, monitor_record),
  case Guard of
    true -> {{Year, Month, Day}, _} = Date,
      List = maps:to_list(maps:filter(fun({_, _, {{Year2, Month2, Day2}, {_, _, _}}, Type2}, _) ->
        (Year2 == Year) and (Month2 == Month) and (Day2 == Day) and (Type2 == Type) end, Monitor#monitor.measures)),
      {value, calculateAvg(List, 0, 0)};
    _ -> {error, "Bad arguments in function getDailyMean(Date, Type, Monitor)."}
  end.

getCorrelation(Type1, Type2, Monitor) ->
  Guard = validateArgument(Type1, string_value) and validateArgument(Type2, string_value),
  case Guard of
    true -> List1 = maps:to_list(maps:filter(fun({_, _, _, Type}, _) -> Type1 == Type end, Monitor#monitor.measures)),
      List2 = maps:to_list(maps:filter(fun({_, _, _, Type}, _) -> Type2 == Type end, Monitor#monitor.measures)),
      Len1 = length(List1),
      Len2 = length(List2),
      if
        (Len1 =< 0) or (Len2 =< 0) -> {error, "Unable to measure correlation, one list is empty"};
        true -> case Len1 >= Len2 of
                  true -> {value, calculateStd(List1, List2)};
                  false -> {value, calculateStd(List2, List1)}
                end
      end;
    _ -> {error, "Bad arguments in function getCorrelation(Type1, Type2, Monitor)."}
  end.

calculateStd(Longer, Shorter) ->
  Diffs = [V#measurement.value - valueFromShorter(Shorter, Key) || {{Key, _, _, _}, V} <- Longer, isInList(Shorter, Key)],
  Sum = lists:foldl(fun(X, Acc) -> Acc + X end, 0, Diffs),
  Len = length(Diffs),
  Avg = Sum / length(Diffs),
  Stdsum = lists:foldl(fun(X, Acc) -> Acc + math:pow(X - Avg, 2) end, 0, Diffs),
  if
    Len == 1 -> math:sqrt(Stdsum);
    true -> math:sqrt(1 / (Len - 1) * Stdsum) * getCEstimator(Len)
  end.

isInList(List, Target) ->
  Res = [Key || {{Key, _, _, _}, _} <- List, Target == Key],
  length(Res) > 0.

valueFromShorter(Shorter, Key) ->
  Tmp = lists:filter(fun({{Key1, _, _, _}, _}) -> Key1 == Key end, Shorter),
  if
    length(Tmp) == 1 ->
      [{_, V}] = Tmp,
      V#measurement.value;
    true -> 0
  end.

calculateAvg([], _, _) ->
  0;
calculateAvg([{_, V}], Sum, Ind) ->
  Sum2 = Sum + V#measurement.value,
  Ind2 = Ind + 1,
  Sum2 / Ind2;
calculateAvg([{_, V} | T], Sum, Ind) ->
  Sum2 = Sum + V#measurement.value,
  Ind2 = Ind + 1,
  calculateAvg(T, Sum2, Ind2).

validateArgument(Argument, Type) ->
  case Type of
    string_value -> case io_lib:char_list(Argument) of
                      true -> (length(Argument) > 0);
                      _ -> false
                    end;
    integer -> is_integer(Argument);
    float_value -> is_float(Argument);
    monitor_record -> is_record(Argument, monitor);
    station_record -> is_record(Argument, station);
    date -> calendar:valid_date(Argument);
    float_tuple -> if
                     is_tuple(Argument) ->
                       {N, E} = Argument,
                       is_float(N) and is_float(E);
                     true -> false
                   end
  end.

getCEstimator(N) ->
  if
    N < 12 -> lists:nth(N - 1, [0.79788, 0.88623, 0.92132, 0.93999, 0.95153, 0.95937, 0.96503,
      0.96931, 0.97266, 0.97535, 0.97756, 0.97941]);
    true -> 1
  end.
