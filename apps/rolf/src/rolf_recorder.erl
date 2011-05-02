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
-export([config/0, is_recorder/0, start_link/0, stop/0, store/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-include("rolf.hrl").

-define(SERVICES_CONFIG_FILE, filename:join("etc", "services.config")).

%% ===================================================================
%% API
%% ===================================================================

%% @doc Return true if current node is a recorder.
is_recorder() ->
    case application:get_key(recorders) of
        {ok, Recorders} ->
            lists:member(node(), Recorders);
        undefined ->
            false
    end.

%% @doc Start a recorder on this node.
start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

%% @doc Stop recorder.
stop() ->
    gen_server:call({global, ?MODULE}, stop).

%% @doc Pass samples to all recorders in the cluster.
store(Sample) ->
    gen_server:cast({global, ?MODULE}, {store, Sample}).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    process_flag(trap_exit, true),
    Config = rolf_recorder:config(),

    % start errd_server
    case errd_server:start_link() of
        {ok, RRD} ->
            Collectors = parse_collector_config(Config),
            start_collectors(RRD, Collectors),
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
%% is a collector and this is the highest priority live recorder (first in the
%% list from app.config).
handle_info({nodeup, Node}, #recorder{collectors=Collectors, rrd=RRD}=State) ->
    log4erl:info("Node ~p up", [Node]),
    Primary = hd(live_recorders()),
    case node() of
        Primary ->
            case lists:keyfind(Node, 1, Collectors) of
                {N, Ss} ->
                    log4erl:info("Configured node ~p", [Node]),
                    start_services(N, Ss, RRD)
            end;
        _ ->
            noop
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
    {ok, Config} = file:consult(?SERVICES_CONFIG_FILE),
    Config.

%% @doc Parse node service definitions from services.config. Return list of
%% {Node, Service} tuples.
%% @spec parse_collector_config([term()]) -> [{node(), [{Plugin, Name, Opts}]}]
parse_collector_config(Defs) ->
    parse_collector_config(Defs, []).

%% @doc Parse node service defintions and populate accumulator. Don't use this
%% function, call parse_collector_config/1.
parse_collector_config([{node, Node, Services}|Defs], Acc) ->
    Acc1 = [{Node, normalise_service_config(Services)}|Acc],
    parse_collector_config(Defs, Acc1);
parse_collector_config([_|Defs], Acc) ->
    parse_collector_config(Defs, Acc).

%% @doc Normalise various short-hand versions of service definition accepted in
%% services.config.
normalise_service_config(Plugin) when is_atom(Plugin) ->
    {Plugin, Plugin, []};
normalise_service_config({Plugin}) ->
    {Plugin, Plugin, []};
normalise_service_config({Plugin, Opts}) when is_list(Opts) ->
    {Plugin, Plugin, Opts};
normalise_service_config({Plugin, Name}) when is_atom(Name) ->
    {Plugin, Name, []};
normalise_service_config({Plugin, Name, Opts}) when is_atom(Name) and is_list(Opts) ->
    {Plugin, Name, Opts}.

%% @doc Return a list of live recorders.
live_recorders() ->
    case application:get_key(recorders) of
        {ok, Recorders} ->
            [R || R <- Recorders, net_adm:ping(R) =:= pong];
        undefined ->
            undefined
    end.

%% @doc Ping collector nodes and give them service configuration.
start_collectors(RRD, Collectors) ->
    LiveCollectors = connect_cluster(Collectors),
    log4erl:info("Starting collectors: ~p", [LiveCollectors]),
    lists:foreach(fun({N, Ss}) -> start_services(RRD, N, Ss) end, LiveCollectors).

%% @doc Ping nodes that we're expected to record from.
connect_cluster(Config) ->
    [{N, S} || {N, S} <- Config, net_adm:ping(N) =:= pong].

%% @doc Start collectors on a set of nodes.
start_services(RRD, Node, SDefs) ->
    Services = [rolf_plugin:load(Name, Plugin, Opts) || {Name, Plugin, Opts} <- SDefs],
    lists:foreach(fun(S) -> rolf_rrd:ensure(RRD, Node, S) end, Services),
    rpc:call(Node, rolf_collector_sup, start_services, [Services]).
