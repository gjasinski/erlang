-module(pollution_gen).
-version('1.0').
-behaviour(gen_server).

-export([start_link/0,init/1, handle_cast/2, handle_call/3, terminate/2]).
-export([addStation/2,
  addValue/4,
  removeValue/3,
  getOneValue/3,
  getDailyMean/2,
  getStationMean/2,
  getMinimumPollutionStation/2,
	crash/0,
	stop/0,
  getInfo/0
]).

start_link() ->
	gen_server:start_link({local, pollution_gen}, pollution_gen, pollution:createMonitor(), []).

init(Monitor) ->
	{ok, Monitor}.

addStation(Name, Coord) ->
	gen_server:cast(pollution_gen, {addStation, Name, Coord}).

addValue(Id, Datetime, Type, Value) ->
	gen_server:cast(pollution_gen, {addValue, Id, Datetime, Type, Value}).

removeValue(Id, Datetime, Type) ->
	gen_server:cast(pollution_gen, {removeValue, Id, Datetime, Type}).

getOneValue(Id, Datetime, Type) ->
	gen_server:call(pollution_gen, {getOneValue,Id, Datetime, Type}).

getDailyMean(Date, Type) ->
	gen_server:call(pollution_gen, {getDailyMean,Date, Type}).

getStationMean(Id, Type) ->
	gen_server:call(pollution_gen, {getStationMean, Id, Type}).

getMinimumPollutionStation(Date, Type) ->
	gen_server:call(pollution_gen, {getMinimumPollutionStation,Date, Type}).

getInfo() ->
  gen_server:call(pollution_gen, {getInfo}).

crash() ->
	gen_server:cast(pollution_gen, crash).

stop() ->
	gen_server:cast(pollution_gen, stop).

handle_cast({addStation, Name, Coord}, Monitor) ->
	{noreply, pollution:addStation(Name, Coord, Monitor)};

handle_cast({addValue, Id, Datetime, Type, Value},Monitor) ->
	{noreply, pollution:addValue(Id, Datetime, Type, Value, Monitor)};

handle_cast({removeValue, Id, Datetime, Type}, Monitor) ->
	{noreply, pollution:removeValue(Id, Datetime, Type, Monitor)};

handle_cast(stop, Monitor) ->
 {stop, normal, Monitor};

handle_cast(crash, Monitor) ->
 1/0,
 {noreply, Monitor}.

handle_call({getOneValue, Id, Datetime, Type}, _, Monitor) ->
	{reply, pollution:getOneValue(Id, Datetime, Type, Monitor), Monitor};

handle_call({getDailyMean, Date, Type}, _, Monitor) ->
	{reply, pollution:getDailyMean(Date, Type, Monitor), Monitor};

handle_call({getStationMean, Id, Type}, _, Monitor) ->
	{reply, pollution:getStationMean(Id, Type, Monitor), Monitor};

handle_call({getMinimumPollutionStation, Date, Type}, _, Monitor) ->
	{reply, pollution:getMinimumPollutionStation(Date, Type, Monitor), Monitor};

handle_call({getInfo}, _, Monitor) -> {reply, Monitor, Monitor}.

terminate(Reason, _Value) ->
  io:format("Server stopped.~n"),
  Reason.
