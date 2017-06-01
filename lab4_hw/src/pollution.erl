-module(pollution).
-author("gjasinski").
-record(station, {name, coord, measurements}).
-record(measurement, {datetime, type, value}).

%% API
-export([createMonitor/0,
  addStation/3,
  addValue/5,
  removeValue/4,
  getOneValue/4,
  getDailyMean/3,
  getStationMean/3,
  getMinimumPollutionStation/3
]).

createMonitor() ->
  #{stationsByName => # { }, stationsByCoord => # {}}.

addStation(Name, Coord, Monitor) ->
  StationsByCoordOld = maps:get(stationsByCoord, Monitor),
  StationsByNameOld = maps:get(stationsByName, Monitor),
  IsKeyName = maps:is_key(Name, StationsByNameOld),
  IsKeyCoord = maps:is_key(Coord, StationsByCoordOld),
  if
    (not IsKeyName) and (not IsKeyCoord) ->
      Station = #station{name = Name, coord = Coord, measurements = []},
      StationsByNameNew = StationsByNameOld#{Name => Station},
      StationsByCoordNew = StationsByCoordOld#{Coord => Station},
      Monitor#{stationsByName => StationsByNameNew, stationsByCoord => StationsByCoordNew};
    true -> Monitor
  end.

addValue(Id, Datetime, Type, Value, Monitor) ->
  StationOld = getStation(Id, Monitor),
  if
    StationOld == false -> Monitor;
    true ->
      MeasurementInList = doesListContainsMeasurement(Datetime, Type, StationOld#station.measurements),
      if
        MeasurementInList -> Monitor;
        true ->
          Measurement = #measurement{datetime = Datetime, type = Type, value = Value},
          MeasurementNew = StationOld#station.measurements ++ [Measurement],
          StationNew = StationOld#station{measurements = MeasurementNew},
          insertStationToMonitor(StationNew, Monitor)
      end
  end.

doesListContainsMeasurement(_, _, []) -> false;
doesListContainsMeasurement(Datetime, Type, [Head|Tail]) ->
  if
    (Datetime == Head#measurement.datetime) and (Type == Head#measurement.type) -> true;
    true -> doesListContainsMeasurement(Datetime, Type, Tail)
  end.

getStation({X, Y}, Monitor) ->
  StationsByCoordOld = maps:get(stationsByCoord, Monitor),
  DoesMapContainCoords = maps:is_key({X, Y}, StationsByCoordOld),
  if
    DoesMapContainCoords -> maps:get({X, Y}, StationsByCoordOld);
    true -> false
  end;

getStation(Name, Monitor) ->
  StationsByNameOld = maps:get(stationsByName, Monitor),
  DoesMapContainName = maps:is_key(Name, StationsByNameOld),
  if
    DoesMapContainName -> maps:get(Name, StationsByNameOld);
    true -> false
  end.

insertStationToMonitor(StationNew, Monitor) ->
  StationsByCoordOld = maps:get(stationsByCoord, Monitor),
  StationsByNameOld = maps:get(stationsByName, Monitor),
  StationsByCoordNew = StationsByCoordOld#{StationNew#station.coord => StationNew},
  StationsByNameNew = StationsByNameOld#{StationNew#station.name => StationNew},
  Monitor#{stationsByName => StationsByNameNew, stationsByCoord => StationsByCoordNew}.

removeValue(Id, Datetime, Type, Monitor) ->
  StationOld = getStation(Id, Monitor),
  MeasurementList = removeMeasurementFromList(Datetime, Type, StationOld#station.measurements),
  StationNew = StationOld#station{measurements = MeasurementList},
  insertStationToMonitor(StationNew, Monitor).

removeMeasurementFromList(_, _, []) -> [];
removeMeasurementFromList(Datetime, Type, [Head|Tail]) ->
  if
    (Datetime == Head#measurement.datetime) and (Type == Head#measurement.type) -> removeMeasurementFromList(Datetime, Type, Tail);
    true -> [Head] ++ removeMeasurementFromList(Datetime, Type, Tail)
  end.

getOneValue(Id, Datetime, Type, Monitor) ->
  Station = getStation(Id, Monitor),
  findMeasurement(Datetime, Type, Station#station.measurements).

findMeasurement(Datetime, Type, [Head|Tail]) ->
  if
    (Datetime == Head#measurement.datetime) and (Type == Head#measurement.type) -> Head#measurement.value;
    true -> findMeasurement(Datetime, Type, Tail)
  end.

getStationMean(Id, Type, Monitor) ->
  Station = getStation(Id, Monitor),
  computeStationMean(Type, Station#station.measurements, 0, 0).


computeStationMean(_, [], _, 0) -> 0;
computeStationMean(_, [], Sum, Strength) -> Sum / Strength;
computeStationMean(Type, [Head|Tail], Sum, Strength) ->
  if
    Type == Head#measurement.type -> computeStationMean(Type, Tail, Sum + Head#measurement.value, Strength + 1);
    true -> computeStationMean(Type, Tail, Sum, Strength)
  end.

getDailyMean(Date, Type,  Monitor) ->
  StationList = maps:to_list(maps:get(stationsByCoord, Monitor)),
  ListOfMeasurementLists = lists:map(fun({_, Station}) -> Station#station.measurements end, StationList),
  computeDailyMeanAllStations(Date, Type, ListOfMeasurementLists, 0, 0).

computeDailyMeanAllStations(_, _, [], _, 0) -> 0;
computeDailyMeanAllStations(_, _, [], Sum, Strength) -> Sum / Strength;
computeDailyMeanAllStations(Date, Type, [Head|Tail], Sum, Strength) ->
  computeDailyMeanAllStations(Date, Type, Tail, Sum + computeDailyMean(Date, Type, Head, 0, 0), Strength + 1).

computeDailyMean(_, _, [], _, 0) -> 0;
computeDailyMean(_, _, [], Sum, Strength) -> Sum / Strength;
computeDailyMean(Date, Type, [Head|Tail], Sum, Strength)->
  {DateFromData, _} = Head#measurement.datetime,
  if
    (Date == DateFromData) and (Type == Head#measurement.type) -> computeDailyMean(Date, Type, Tail, Sum + Head#measurement.value, Strength + 1);
    true -> computeDailyMean(Date, Type, Tail, Sum, Strength)
  end.

getMinimumPollutionStation(Date, Type, Monitor) ->
  StationList = lists:map(fun({_, Station}) -> Station end, maps:to_list(maps:get(stationsByCoord, Monitor))),
  {_, Id, Coord, _} = findMinimumPollutionStation(Date, Type, StationList, 0, "-"),
  {Id, Coord}.

findMinimumPollutionStation(_, _, [], _, Station) -> Station;
findMinimumPollutionStation(Date, Type, [Head|Tail], _, "-") ->
  MinComputedValue = findMinimumValue(Date, Type, Head#station.measurements, "-"),
  findMinimumPollutionStation(Date, Type, Tail, MinComputedValue, Head);
findMinimumPollutionStation(Date, Type, [Head|Tail], MinValue, Station) ->
  MinComputedValue = findMinimumValue(Date, Type, Head#station.measurements, "-"),
  if
    MinComputedValue < MinValue -> findMinimumPollutionStation(Date, Type, Tail, MinComputedValue, Head);
    true -> findMinimumPollutionStation(Date, Type, Tail, MinValue, Station)
  end.

findMinimumValue(_, _, [], Value) -> Value;
findMinimumValue(Date, Type, [Head|Tail], "-") ->
  {DateFromData, _} = Head#measurement.datetime,
  if
    (Date == DateFromData) and (Type == Head#measurement.type) -> findMinimumValue(Date, Type, Tail, Head#measurement.value);
    true -> findMinimumValue(Date, Type, Tail, "-")
  end;
findMinimumValue(Date, Type, [Head|Tail], Value) ->
  {DateFromData, _} = Head#measurement.datetime,
  if
    (Date == DateFromData) and (Type == Head#measurement.type) and (Head#measurement.value < Value) -> findMinimumValue(Date, Type, Tail, Head#measurement.value);
    true -> findMinimumValue(Date, Type, Tail, Value)
  end.

%-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

add_value_and_search_it_test() ->
    Name = "Aleja Słowackiego",
    Time = calendar:local_time(),
    Type = "PM10",
    Value = 42,
    M1 = createMonitor(),
    M2 = addStation(Name, {50.2345, 18.3445}, M1),
    M3 = addValue(Name, Time, Type, Value, M2),
    ?assertEqual(Value, getOneValue(Name, Time, Type, M3)).

add_value_and_delete_it_test() ->
  Name = "Aleja Słowackiego",
  Time = calendar:local_time(),
  Type = "PM10",
  Value = 42,
  M1 = createMonitor(),
  M2 = addStation(Name, {50.2345, 18.3445}, M1),
  M3 = addValue(Name, Time, Type, Value, M2),
  M4 = removeValue(Name, Time, Type, M3),
  ?assertException(error, function_clause, getOneValue(Name, Time, Type, M4)).

add_duplicated_station_test() ->
  Name = "Aleja Słowackiego",
  M1 = createMonitor(),
  M2 = addStation(Name, {50.2345, 18.3445}, M1),
  M3 = addStation(Name, {50.2345, 18.3445}, M2),
  ?assertEqual(M2, M3).

add_duplicated_measurements_test() ->
  Name = "Aleja Słowackiego",
  Time = calendar:local_time(),
  Type = "PM10",
  Value = 42,
  M1 = createMonitor(),
  M2 = addStation(Name, {50.2345, 18.3445}, M1),
  M3 = addValue(Name, Time, Type, Value, M2),
  M4 = addValue(Name, Time, Type, Value, M3),
  ?assertEqual(M3, M4).

getStationMean_test() ->
  Name = "Aleja Słowackiego",
  Name2 = "Kawiory",
  Time = calendar:local_time(),
  Time2 = {{2000,5,4},{19,55,50}},
  Time3 = {{2000,5,4},{20,55,50}},
  Time4 = {{2000,5,4},{21,55,50}},
  M1 = createMonitor(),
  M2 = addStation(Name, {50.2345, 18.3445}, M1),
  M3 = addValue(Name, Time, "M1", 10, M2),
  M4 = addValue(Name, Time, "M2", 15, M3),
  M5 = addValue(Name, Time, "M3", 20, M4),
  M6 = addValue(Name, Time2, "M1", 0, M5),
  M7 = addValue(Name, Time3, "M1", 100, M6),
  M8 = addValue(Name, Time4, "M1", 30, M7),
  M9 = addValue(Name, Time2, "M2", 9, M8),
  ?assertEqual(35.0, getStationMean(Name, "M1", M9)),
  M10 = addStation(Name2, {50.0, 18.0}, M9),
  M11 = addValue(Name2, Time4, "M1", 30, M10),
  ?assertEqual(35.0, getStationMean(Name, "M1", M11)),
  ?assertEqual(30.0, getStationMean(Name2, "M1", M11)).

getDailyMean_test() ->
  Name1 = "Aleja Słowackiego",
  Name2 = "Kawiory",
  Name3 = "Wawel",
  Time1 = {{2000,5,4},{19,55,50}},
  Time2 = calendar:local_time(),

  M1 = createMonitor(),
  M2 = addStation(Name1, {50.2345, 18.3445}, M1),
  M3 = addStation(Name2, {50.23451, 18.34451}, M2),
  M4 = addStation(Name3, {50.23452, 18.34452}, M3),

  M5 = addValue(Name1, Time1, "M1", 20, M4),
  M6 = addValue(Name1, Time2, "M1", 0, M5),
  M7 = addValue(Name1, Time1, "M2", 100, M6),
  M8 = addValue(Name1, Time2, "M2", 30, M7),

  M9 = addValue(Name2, Time1, "M1", 100, M8),
  M10 = addValue(Name2, Time1, "M2", 120, M9),
  M11 = addValue(Name3, Time1, "M1", 210, M10),
  ?assertEqual(110.0, getDailyMean({2000,5,4}, "M1", M11)).

getMinimumPollutionStation_test() ->
  Name1 = "Aleja Słowackiego",
  Name2 = "Kawiory",
  Name3 = "Wawel",
  Time1 = {{2000,5,4},{19,55,50}},
  Time2 = calendar:local_time(),

  M1 = createMonitor(),
  M2 = addStation(Name1, {50.2345, 18.3445}, M1),
  M3 = addStation(Name2, {50.23451, 18.34451}, M2),
  M4 = addStation(Name3, {50.23452, 18.34452}, M3),

  M5 = addValue(Name1, Time1, "M1", 20, M4),
  M6 = addValue(Name1, Time2, "M1", 0, M5),
  M7 = addValue(Name1, Time1, "M2", 100, M6),
  M8 = addValue(Name1, Time2, "M2", 30, M7),

  M9 = addValue(Name2, Time1, "M1", 100, M8),
  M10 = addValue(Name2, Time1, "M2", 120, M9),

  M11 = addValue(Name3, Time1, "M1", 210, M10),
  ?assertMatch({Name1,_}, getMinimumPollutionStation({2000,5,4}, "M1", M11)).

%-endif.
