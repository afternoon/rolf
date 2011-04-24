%% @doc gen_server to which services can send samples for recording.
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

-module(rolf_recorder).
-behaviour(gen_server).

%% API
-export([config/0, is_recorder/2, start_link/1, stop/0, store/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-include("rolf.hrl").

-define(RECORDER_CONFIG_FILE, filename:join("priv", "recorder.config")).

%% ===================================================================
%% API
%% ===================================================================

is_recorder(Config, Node) ->
    Recorders = proplists:get_value(recorders, Config, [node()]),
    lists:member(Node, Recorders).

%% @doc Start a recorder on this node.
start_link(Config) ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [Config], []).

%% @doc Stop recorder.
stop() ->
    gen_server:call({global, ?MODULE}, stop).

%% @doc Pass samples to all recorders in the cluster.
store(Sample) ->
    gen_server:cast({global, ?MODULE}, {store, Sample}).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([Config]) ->
    process_flag(trap_exit, true),

    % start errd_server
    case errd_server:start_link() of
        {ok, RRD} ->
            Collectors = collectors(Config),
            start_collectors(Collectors, RRD),
            net_kernel:monitor_nodes(true),
            {ok, #recorder{collectors=Collectors, rrd=RRD}};
        Else ->
            error_logger:error_report([{where, {node(), rolf_recorder, init}}, {errd_server_error, Else}]),
            {stop, Else}
    end.

handle_call(_Req, _From, State) ->
  {reply, State}.

handle_cast({store, Sample}, #recorder{rrd=RRD}=State) ->
    error_logger:info_report([{where, {node(), rolf_recorder, handle_cast, store}}, {sample, Sample}]),
    rolf_rrd:update(RRD, Sample),
    {noreply, State}.

%% @doc Handle nodeup messages from monitoring nodes. Start services if the node
%% is a collector.
handle_info({nodeup, Node}, #recorder{collectors=Collectors, rrd=RRD}=State) ->
    error_logger:info_report([{where, {node(), rolf_recorder, handle_info, nodeup}}, {node, Node}, {collectors, Collectors}]),
    case lists:keyfind(Node, 1, Collectors) of
        {N, Ss} -> start_services(N, Ss, RRD)
    end,
    {noreply, State};

handle_info({nodedown, Node}, State) ->
    error_logger:info_report([{where, {node(), rolf_recorder, handle_info, nodedown}}, {node, Node}]),
    {noreply, State}.

terminate(_Reason, #recorder{rrd=RRD}) ->
    errd_server:stop(RRD).

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ===================================================================
%% Utility functions
%% ===================================================================

%% @doc Load configuration of recorders, collectors and services.
config() ->
    case file:consult(?RECORDER_CONFIG_FILE) of
        {ok, Config} -> Config;
        Else -> Else
    end.

%% @doc Get node and service config from config file.
collectors(Config) ->
    AllSNames = rolf_plugin:list(),
    ServiceConfig = parse_collector_config(Config),
    [{N, expand(SNames, AllSNames)} || {N, SNames} <- ServiceConfig].

%% @doc Parse recorder.config {service} definitions. Return list of
%% {Node, Service} tuples.
parse_collector_config(Config) ->
    ServiceDefs = [{Name, Nodes} || {service, Name, Nodes, _Opts} <- Config],
    parse_service_config(ServiceDefs, []).

parse_service_config([{Name, Nodes}|Services], Acc) ->
    Acc1 = parse_node_config(Name, Nodes, Acc),
    parse_service_config(Services, Acc1).

parse_node_config(Name, [Node|Nodes], Acc) ->
    Acc1 = case proplists:get_value(Node, Acc) of
        undefined ->
            [{Node, [Name]}|Acc];
        {Node, Names} ->
            [{Node, [Name|Names]}|proplists:delete(Node, Acc)]
    end,
    parse_node_config(Name, Nodes, Acc1);
parse_node_config(_Name, [], Acc) ->
    Acc.

%% @doc Expand special all keyword.
expand(Some, All) ->
    case Some of
        all -> All;
        _ ->   Some
    end.

%% @doc Ping collector nodes and give them service configuration.
start_collectors(Collectors, RRD) ->
    LiveCollectors = connect_cluster(Collectors),
    error_logger:info_report([{where, {node(), rolf_recorder, start_collectors}}, {live_collectors, LiveCollectors}]),
    lists:foreach(fun({N, Ss}) -> start_services(N, Ss, RRD) end, LiveCollectors).

%% @doc Ping nodes that we're expected to record from.
connect_cluster(Config) ->
    [{N, S} || {N, S} <- Config, net_adm:ping(N) =:= pong].

%% @doc Start collectors on a set of nodes.
start_services(Node, SNames, RRD) ->
    error_logger:info_report([{where, {node(), rolf_recorder, start_services}}, {node, Node}, {snames, SNames}]),
    Services = [rolf_plugin:load(SN) || SN <- SNames],
    lists:foreach(fun(S) -> rolf_rrd:ensure(RRD, Node, S) end, Services),
    rpc:call(Node, rolf_collector_sup, start_services, [Services]).

%% ===================================================================
%% Tests
%% ===================================================================

expand_test() ->
    All = [disk, loadtime],
    ?assertEqual([loadtime], expand([loadtime], All)),
    ?assertEqual([disk, loadtime], expand(all, All)).
