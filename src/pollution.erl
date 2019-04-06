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
-export([createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/3]).

createMonitor() ->
  #monitor{}.

addStation(Name, {N, E}, Monitor)
  when is_record(Monitor, monitor) ->
  case maps:is_key(Name, Monitor#monitor.stations) or maps:is_key({N, E}, Monitor#monitor.stations) of
    true -> error_logger:error_msg("Unable to addStation, station already exists.");
    false ->
      Station = #station{name = Name, coordinates = {N, E}, measures = #{}},
      Stations = (Monitor#monitor.stations)#{Name => Station, {N, E} => Station},
      #monitor{stations = Stations}
  end.

addValue(Key, Date, Type, Value, Monitor)
  when is_record(Monitor, monitor) ->
  case maps:is_key(Key, Monitor#monitor.stations) of
    true ->
      Stations = Monitor#monitor.stations,
      Station = maps:get(Key, Stations),
      Measures = Station#station.measures,
      case maps:is_key({Date, Type}, Measures) of
        true -> error_logger:error_msg("Unable to add measurement, measurement already exista.");
        false ->
          NewMeasures = Measures#{{Date, Type} => #measurement{value = Value}},
          StationName = Station#station.name,
          StationCoordinates = Station#station.coordinates,
          NewStation = #station{name = StationName, coordinates = StationCoordinates, measures = NewMeasures},
          NewStations = Stations#{StationName => NewStation, StationCoordinates => NewStation},
          #monitor{stations = NewStations}
      end;
    false -> error_logger:error_msg("Unable to add measurement, station doesn't exist.")
  end.

removeValue(Key, Date, Type, Monitor)
  when is_record(Monitor, monitor) ->
  case maps:is_key(Key, Monitor#monitor.stations) of
    true ->
      Stations = Monitor#monitor.stations,
      Station = maps:get(Key, Stations),
      Measures = Station#station.measures,
      case maps:is_key({Date, Type}, Measures) of
        true ->
          NewMeasures = maps:remove({Date, Type}, Measures),
          StationName = Station#station.name,
          StationCoordinates = Station#station.coordinates,
          NewStation = #station{name = StationName, coordinates = StationCoordinates, measures = NewMeasures},
          NewStations = Stations#{StationName => NewStation, StationCoordinates => NewStation},
          #monitor{stations = NewStations};
        false ->
          error_logger:error_msg("Unable to remove measurement, measurement doesn't exist.")
      end;
    false -> error_logger:error_msg("Unable to remove measurement, station doesn't exist.")
  end.

getOneValue(Key, Date, Type, Monitor)
  when is_record(Monitor, monitor) ->
  case maps:is_key(Key, Monitor#monitor.stations) of
    true ->
      Stations = Monitor#monitor.stations,
      Station = maps:get(Key, Stations),
      Measures = Station#station.measures,
      case maps:is_key({Date, Type}, Measures) of
        true -> maps:get({Date, Type}, Measures);
        false -> error_logger:error_msg("Unable to return measurement, measurement doesn't exist.")
      end;
    false -> error_logger:error_msg("Unable to return measurement, station doesn't exist.")
  end.

getStationMean(Key, Type, Monitor)
  when is_record(Monitor, monitor) ->
  case maps:is_key(Key, Monitor#monitor.stations) of
    true ->
      Stations = Monitor#monitor.stations,
      Station = maps:get(Key, Stations),
      Measures = Station#station.measures,
      List = maps:to_list(maps:filter(fun({_, Type2}, _) -> Type2 == Type end, Measures)),
      calculateAvg(List, 0, 0);
    false -> error_logger:error_msg("Unable to calculate mean, station doesn't exist.")
  end.

getDailyMean(Date, Type, Monitor)
  when is_record(Monitor, monitor) ->
  {{Year, Month, Day}, _} = Date,
  Fun = fun({{Year2, Month2, Day2}, {_, _, _}}, V, Acc) when Year2 == Year and Month2 == Month and Day2 == Day ->
    Acc = Acc + V#measurement.value end,
  Sum = maps:fold(Fun, 0, )

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