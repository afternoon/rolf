%% @doc Rolf app startup.
%% @author Ben Godfrey <ben@ben2.com> [http://aftnn.org/]
%% @copyright 2011 Ben Godfrey
%% @version 1.0.0
%%
%% Rolf - a monitoring and graphing tool like Munin or collectd.
%% Copyright (C) 2011 Ben Godfrey.
%%
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with this program. If not, see <http://www.gnu.org/licenses/>.

-module(rolf_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    start(normal, undefined).

start(_StartType, _StartArgs) ->
    configure_logger(),
    log4erl:info("Starting"),
    CResult = rolf_collector_sup:start_link(),
    case rolf_recorder:is_recorder() of
        true ->
            log4erl:info("~p is a recorder", [node()]),
            rolf_recorder_sup:start_link();
        _ ->
            log4erl:info("~p is a collector only", [node()]),
            announce_collector(),
            CResult
    end.

stop(_State) ->
    ok.

%% ===================================================================
%% Utility functions
%% ===================================================================

%% @doc Load log4erl configuration.
configure_logger() ->
    {ok, ConfigFilename} = application:get_env(log4erl_config),
    log4erl:conf(ConfigFilename).

%% @doc Announce collector start to recorders.
announce_collector() ->
    lists:foreach(fun net_adm:ping/1, rolf_recorder:recorders()).
