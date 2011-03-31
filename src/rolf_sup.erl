%% @doc Rolf supervisor configuration.
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

-module(rolf_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

-include("rolf.hrl").

-define(SERVICE_CONFIG_FILE, filename:join("priv", "services.config")).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({global, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%% @doc Master supervisor for rolf application.
init([]) ->
    ServiceConfig = service_config(),
    LiveServiceConfig = connect_cluster(ServiceConfig),

    % start a rolf_service_sup on each node

    Recorder = {rolf_recorder, {rolf_recorder, start_link, []},
                permanent, 5000, worker, [rolf_recorder]},
    ChildSpecs = [Recorder|[child_spec(N, Ss) || {N, Ss} <- LiveServiceConfig]],
    {ok, {{one_for_one, 5, 60}, ChildSpecs}}.

%% ===================================================================
%% Utility functions
%% ===================================================================

service_config() ->
    case file:consult(?SERVICE_CONFIG_FILE) of
        {ok, Config} ->
            AllSNames = rolf_plugin:list(),
            [{N, expand_snames(SNames, AllSNames)} || {N, SNames} <- Config];
        Else -> Else
    end.

%% @doc Expand special all keyword in service configuration for a node.
expand_snames(SNames, All) ->
    case SNames of
        all -> All;
        _ ->   SNames
    end.

%% @doc Connect the cluster by pinging all the nodes we want to record samples
%% from. Return the list of live nodes.
connect_cluster(Config) ->
    [{N, S} || {N, S} <- Config, net_adm:ping(N) =:= pong].

child_spec(_Node, Services) ->
    {rolf_service_sup, {rolf_service_sup, start_link, [Services]},
                       permanent, infinity, supervisor, [rolf_service_sup]}.

%% ===================================================================
%% Tests
%% ===================================================================

expand_snames_test() ->
    All = [disk, loadtime],
    ?assertEqual([loadtime], expand_snames([loadtime], All)),
    ?assertEqual([disk, loadtime], expand_snames(all, All)).

node_service_pairs_test() ->
    ?assertEqual([{x, 1}, {y, 1}, {y, 2}], [{x, [1]}, {y, [1, 2]}]).
