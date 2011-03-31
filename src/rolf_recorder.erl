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
-export([start_link/0, stop/0, store/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-include("rolf.hrl").

-define(CONFIG_FILE, filename:join("priv", "services.config")).

-define(ERRD_SERVER_CHILD_SPEC,
        {rolf_sup, {errd_server, start_link, []},
                   permanent, infinity, worker, [errd_server]}).

%% ===================================================================
%% API
%% ===================================================================

%% @doc Start a recorder on this node.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Stop recorder.
stop() -> gen_server:call(?MODULE, stop).

%% @doc Pass samples to all recorders in the cluster.
store(Sample) -> gen_server:abcast(?MODULE, {store, Sample}).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    process_flag(trap_exit, true),
    error_logger:info_report({rolf_recorder, init}),

    % load recorder config
    State = load_config(),

    % create list of all available services
    AllServiceNames = rolf_plugin:list(),

    % start errd server and add it to the supervisor
    case errd_server:start_link() of
        {ok, RRD} ->
            % start services on the cluster
            % register(errd_server, RRD),
            start_nodes(RRD, AllServiceNames, State#recorder.nodes),
            {ok, State#recorder{rrd=RRD}};
        Else ->
            error_logger:error_report({rolf_recorder, errd_server, error, Else}),
            Else
    end.

handle_call(get_state, _From, State) ->
    error_logger:info_report({rolf_recorder, get_state, State}),
    {reply, State, State}.

handle_cast({store, Sample}, #recorder{rrd=RRD}=State) ->
    error_logger:info_report({rolf_recorder, store, Sample}),
    rolf_rrd:update(RRD, Sample),
    {noreply, State};

handle_cast(Msg, State) ->
    error_logger:info_report({rolf_recorder, handle_cast, Msg}),
    {noreply, State}.

handle_info(Info, State) ->
    error_logger:info_report({rolf_recorder, handle_info, Info}),
    {noreply, State}.

terminate(Reason, _State) ->
    error_logger:info_report({rolf_recorder, terminate, Reason}),
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ===================================================================
%% Utility functions
%% ===================================================================

%% @doc Load services configuration from file.
load_config() ->
    error_logger:info_report({rolf_recorder, load_config}),
    {ok, Cfg} = file:consult(?CONFIG_FILE),
    #recorder{nodes=proplists:get_value(nodes, Cfg, [{node(), all}])}.

%% @doc Start services on all connected nodes, rolf_service_sup should do this.
start_nodes(_RRD, _AllSNames, []) -> ok;
start_nodes(RRD, AllSNames, [{Node, SNames}|Nodes]) ->
    case net_adm:ping(Node) of
        pong ->
            Expanded = expand_snames(SNames, AllSNames),
            error_logger:info_report({rolf_recorder, start_nodes, Node, Expanded}),
            Services = lists:map(fun rolf_plugin:load/1, Expanded),
            lists:foreach(fun(S) -> rolf_rrd:ensure(RRD, Node, S) end, Services),
            rpc:call(Node, rolf_recorder, start_services, [Services]);
        pang ->
            error_logger:info_report({rolf_recorder, start_nodes, nodedown, Node}),
            ok
    end,
    start_nodes(RRD, AllSNames, Nodes).

%% @doc Start an instance of the rolf_service gen_server for each service.
%% Called by the recorder when the cluster is constructed.
start_services([]) -> ok;
start_services([SName|Services]) ->
    S = rolf_plugin:load(SName),
    error_logger:info_report({rolf_recorder, node(), start_services, S}),
    Result = rolf_service:start_link(S),
    error_logger:info_report({rolf_recorder, node(), start_services, Result}),
    start_services(Services).

%% @doc Expand special all keyword in service configuration for a node.
expand_snames(SNames, All) ->
    case SNames of
        all -> All;
        _ ->   SNames
    end.

%% ===================================================================
%% Tests
%% ===================================================================

expand_snames_test() ->
    All = [disk, loadtime],
    ?assertEqual([loadtime], expand_snames([loadtime], All)),
    ?assertEqual([disk, loadtime], expand_snames(all, All)).
