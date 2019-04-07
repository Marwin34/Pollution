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
              true -> error_logger:error_msg("Unable to addStation, station already exists.");
              false ->
                Station = #station{name = Name, coordinates = {N, E}},
                Stations = (Monitor#monitor.stations)#{Name => Station, {N, E} => Station},
                #monitor{stations = Stations}
            end;
    _ -> error_logger:error_msg("Bad arguments in function addStation(Name, {N,E}, Monitor).")
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
                  true -> error_logger:error_msg("Unable to add measurement, measurement already exists.");
                  false ->
                    NewMeasures = (Monitor#monitor.measures)#{{StationName, StationCoordinates, Date, Type} =>
                    #measurement{value = Value}},
                    #monitor{stations = Stations, measures = NewMeasures}
                end;
              false -> error_logger:error_msg("Unable to add measurement, station doesn't exist.")
            end;
    _ -> error_logger:error_msg("Bad arguments in function addValue(Key, Date, Type, Value, Monitor).")
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
                    #monitor{stations = Stations, measures = NewMeasures};
                  false ->
                    error_logger:error_msg("Unable to remove measurement, measurement doesn't exist.")
                end;
              false -> error_logger:error_msg("Unable to remove measurement, station doesn't exist.")
            end;
    _ -> error_logger:error_msg("Bad arguments in function removeValue(Key, Date, Type, Monitor).")
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
                  true -> maps:get({StationName, StationCoordinates, Date, Type}, Monitor#monitor.measures);
                  false -> error_logger:error_msg("Unable to return measurement, measurement doesn't exist.")
                end;
              false -> error_logger:error_msg("Unable to return measurement, station doesn't exist.")
            end;
    _ -> error_logger:error_msg("Bad arguments in function getOneValue(Key, Date, Type, Monitor).")
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
                calculateAvg(List, 0, 0);
              false -> error_logger:error_msg("Unable to calculate mean, station doesn't exist.")
            end;
    _ -> error_logger:error_msg("Bad arguments in function getStationMean(Key, Type, Monitor).")
  end.

getDailyMean(Date, Type, Monitor) ->
  Guard = validateArgument(Type, string_value) and validateArgument(Monitor, monitor_record),
  case Guard of
    true -> {{Year, Month, Day}, _} = Date,
      List = maps:to_list(maps:filter(fun({_, _, {{Year2, Month2, Day2}, {_, _, _}}, Type2}, _) ->
        (Year2 == Year) and (Month2 == Month) and (Day2 == Day) and (Type2 == Type) end, Monitor#monitor.measures)),
      calculateAvg(List, 0, 0);
    _ -> error_logger:error_msg("Bad arguments in function getDailyMean(Date, Type, Monitor).")
  end.


getCorrelation(Type1, Type2, Monitor) ->
  Guard = validateArgument(Type1, string_value) and validateArgument(Type2, string_value),
  case Guard of
      true ->List1 = maps:to_list(maps:filter(fun({_, _, _, Type}, _) -> Type1 == Type end, Monitor#monitor.measures)),
        List2 = maps:to_list(maps:filter(fun({_, _, _, Type}, _) -> Type2 == Type end, Monitor#monitor.measures)),
        Len1 = length(List1),
        Len2 = length(List2),
        if
          (Len1 =< 0) or (Len2 =< 0) -> error_logger:error_msg("Unable to measure correlation, one list is empty");
          true -> case Len1 >= Len2 of
                    true -> calculateStd(List1, List2, Len2);
                    false -> calculateStd(List1, List2, Len1)
                  end
        end;
    _ -> error_logger:error_msg("Bad arguments in function getCorrelation(Type1, Type2, Monitor).")
end.

calculateStd(List1, List2, Len) ->
  List3 = lists:sublist(List1, Len),
  List4 = lists:zip(List2, List3),
  Diffs = lists:map(fun({{_, V1}, {_, V2}}) -> abs(V1#measurement.value - V2#measurement.value) end, List4),
  Sum = lists:foldl(fun(X, Acc) -> Acc + X end, 0, Diffs),
  Avg = Sum / Len,
  Stdsum = lists:foldl(fun(X, Acc) -> Acc + math:pow(X - Avg, 2) end, 0, Diffs),
  case Len == 1 of
    true -> math:sqrt(Stdsum);
    false -> math:sqrt(1 / (Len - 1) * Stdsum)
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
    string_value -> (length(Argument) > 0);
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

printMsg(Value) ->
  case Value of
    monitor_error -> error_logger:error_msg("Monitor doesn't exist.");
    station_error -> error_logger:error_msg("Station doesn't exist.");
    meausrement_error -> error_logger:error_msg("Measurement doesn't exist.");
    string_error -> error_logger:error_msg("Empty string.");
    _ -> error_logger:error_msg(Value)
  end.