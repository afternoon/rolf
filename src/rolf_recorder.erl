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
            start_collectors(Config, RRD),
            {ok, #recorder{rrd=RRD}};
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

handle_info(_Info, State) ->
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

%% @doc Expand special all keyword in service configuration for a node.
expand_snames(SNames, All) ->
    case SNames of
        all -> All;
        _ ->   SNames
    end.

%% @doc Ping collector nodes and give them service configuration.
start_collectors(Config, RRD) ->
    NodeServices = service_config(Config),
    LiveNodeServices = connect_cluster(NodeServices),
    error_logger:info_report([{where, {node(), rolf_recorder, start_collectors}}, {services, LiveNodeServices}]),
    start_services(LiveNodeServices, RRD).

%% @doc Get node and service config from config file.
service_config(Config) ->
    AllSNames = rolf_plugin:list(),
    ServiceConfig = proplists:get_value(services, Config, [{node(), all}]),
    [{N, expand_snames(SNames, AllSNames)} || {N, SNames} <- ServiceConfig].

%% @doc Ping nodes that we're expected to record from.
connect_cluster(Config) ->
    [{N, S} || {N, S} <- Config, net_adm:ping(N) =:= pong].

%% @doc Start collectors on a set of nodes.
start_services([], _RRD) -> ok;
start_services([{N, SNames}|NodeServices], RRD) ->
    Services = [rolf_plugin:load(SN) || SN <- SNames],
    lists:foreach(fun(S) -> rolf_rrd:ensure(RRD, N, S) end, Services),
    rpc:call(N, rolf_collector_sup, start_services, [Services]),
    start_services(NodeServices, RRD).

%% ===================================================================
%% Tests
%% ===================================================================

expand_snames_test() ->
    All = [disk, loadtime],
    ?assertEqual([loadtime], expand_snames([loadtime], All)),
    ?assertEqual([disk, loadtime], expand_snames(all, All)).
