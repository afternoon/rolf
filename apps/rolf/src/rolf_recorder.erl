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

-define(RECORDER_CONFIG_FILE, filename:join("etc", "recorder.config")).

%% ===================================================================
%% API
%% ===================================================================

is_recorder(Node, Config) ->
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
            Collectors = parse_collector_config(Config),
            start_collectors(Collectors, RRD),
            net_kernel:monitor_nodes(true),
            {ok, #recorder{collectors=Collectors, rrd=RRD}};
        Else ->
            log4erl:error("ERRD server error: ~p", [Else]),
            {stop, Else}
    end.

handle_call(_Req, _From, State) ->
  {reply, State}.

handle_cast({store, Sample}, #recorder{rrd=RRD}=State) ->
    Service = Sample#sample.service,
    log4erl:debug("~p sample from ~p, values: ~p", [Service#service.name, Sample#sample.node, Sample#sample.values]),
    rolf_rrd:update(RRD, Sample),
    {noreply, State}.

%% @doc Handle nodeup messages from monitoring nodes. Start services if the node
%% is a collector.
handle_info({nodeup, Node}, #recorder{collectors=Collectors, rrd=RRD}=State) ->
    log4erl:info("Node ~p up", [Node]),
    case lists:keyfind(Node, 1, Collectors) of
        {N, Ss} -> start_services(N, Ss, RRD)
    end,
    {noreply, State};

handle_info({nodedown, Node}, State) ->
    log4erl:info("Node ~p down", [Node]),
    {noreply, State}.

terminate(_Reason, #recorder{rrd=RRD}) ->
    errd_server:stop(RRD),
    log4erl:info("Recorder terminated").

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ===================================================================
%% Utility functions
%% ===================================================================

%% @doc Load configuration of recorders, collectors and services.
config() ->
    {ok, Config} = file:consult(?RECORDER_CONFIG_FILE),
    Config.

%% @doc Parse recorder.config {service} definitions. Return list of
%% {Node, Service} tuples.
%% @spec parse_collector_config([term()]) -> [{node(), [{ServiceName, Opts}]}]
parse_collector_config(Config) ->
    ServiceDefs = [{Name, Nodes, Opts} || {service, Name, Nodes, Opts} <- Config],
    parse_service_config(ServiceDefs, []).

parse_service_config([{Name, Nodes, Opts}|Services], Acc) ->
    NodeList = if is_list(Nodes) -> Nodes; true -> [Nodes] end,
    Acc1 = parse_node_config(Name, NodeList, Opts, Acc),
    parse_service_config(Services, Acc1);
parse_service_config([], Acc) ->
    Acc.

parse_node_config(Name, [Node|Nodes], Opts, Acc) ->
    Acc1 = case proplists:get_value(Node, Acc) of
        undefined ->
            [{Node, [{Name, Opts}]}|Acc];
        {Node, ServiceDefs} ->
            [{Node, [{Name, Opts}|ServiceDefs]}|proplists:delete(Node, Acc)]
    end,
    parse_node_config(Name, Nodes, Opts, Acc1);
parse_node_config(_Name, [], _Opts, Acc) ->
    Acc.

%% @doc Ping collector nodes and give them service configuration.
start_collectors(Collectors, RRD) ->
    LiveCollectors = connect_cluster(Collectors),
    log4erl:info("Starting collectors: ~p", [LiveCollectors]),
    lists:foreach(fun({N, Ss}) -> start_services(N, Ss, RRD) end, LiveCollectors).

%% @doc Ping nodes that we're expected to record from.
connect_cluster(Config) ->
    [{N, S} || {N, S} <- Config, net_adm:ping(N) =:= pong].

%% @doc Start collectors on a set of nodes.
start_services(Node, SDefs, RRD) ->
    Services = [rolf_plugin:load(Name, Opts) || {Name, Opts} <- SDefs],
    lists:foreach(fun(S) -> rolf_rrd:ensure(RRD, Node, S) end, Services),
    rpc:call(Node, rolf_collector_sup, start_services, [Services]).
