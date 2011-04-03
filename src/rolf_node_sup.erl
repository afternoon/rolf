%% @doc Rolf node supervisor configuration.
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

-module(rolf_node_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_cluster/0, start_node/2, service_config/0]).

%% supervisor callbacks
-export([init/1]).

-include("rolf.hrl").

-define(SERVICE_CONFIG_FILE, filename:join("priv", "recorder.config")).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({global, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%% @doc Node supervisor builds the cluster and starts a service supervisor on
%% each node.
init([]) ->
    SupFlags = {simple_one_for_one, 3, 5},
    ChildTemplate = {rolf_service_sup,
                     {rolf_service_sup, start_link, []},
                     permanent, 10000, supervisor, [rolf_service_sup]},
    {ok, {SupFlags, [ChildTemplate]}}.

%% @doc Start a rolf_service_sup on each node
start_cluster() ->
    ServiceConfig = service_config(),
    LiveServiceConfig = connect_cluster(ServiceConfig),
    lists:foreach(fun({N, Ss}) -> start_node(N, Ss) end, LiveServiceConfig).

%% @doc Set up a node, start a rolf_service_sup.
start_node(Node, Services) ->
    error_logger:info_report({rolf_node_sup, start_node, Node, Services}),
    rpc:call(Node, supervisor, start_child, [{global, ?MODULE}, [Services]]).

%% ===================================================================
%% Utility functions
%% ===================================================================

%% @doc Load the node, service configuration.
service_config() ->
    case file:consult(?SERVICE_CONFIG_FILE) of
        {ok, Config} ->
            AllSNames = rolf_plugin:list(),
            ServiceConfig = proplists:get_value(services, Config, [{node(), all}]),
            [{N, expand_snames(SNames, AllSNames)} || {N, SNames} <- ServiceConfig];
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

%% ===================================================================
%% Tests
%% ===================================================================

expand_snames_test() ->
    All = [disk, loadtime],
    ?assertEqual([loadtime], expand_snames([loadtime], All)),
    ?assertEqual([disk, loadtime], expand_snames(all, All)).
