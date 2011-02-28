%% @doc gen_server to which services can send samples for recording.
%% @author Ben Godfrey <ben@ben2.com> [http://aftnn.org/]
%% @copyright 2011 Ben Godfrey
%% @version 1.0.0
%%
%% Rolf - a system monitoring and graphing tool like Munin or collectd.
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
-export([
        %api
        store/1,
        % gen_server
        init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
        code_change/3,
        % utils
        subscribe/1]).
-include("rolf.hrl").

-define(RRD_DIR, filename:join("priv", "data")).
-define(RRD_EXT, ".rrd").

%% ===================================================================
%% API
%% ===================================================================

%% @doc Listen for sample messages.
store(Sample) -> gen_server:call(?MODULE, {store, Sample}).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    error_logger:info_report({rolf_recorder, init}),
    ErrdServer = errd_server:start_link(), % supervisor should do this?
    {ok, #recorder{errdserver=ErrdServer}}.

handle_call(get_state, _From, State) ->
    error_logger:info_report({rolf_recorder, get_state, State}),
    {reply, State, State};

handle_call({store, Sample}, _From, State) ->
    error_logger:info_report({rolf_recorder, store, Sample}),
    ensure_rrd(State#recorder.errdserver, 'josie@josie', foo, foos, counter),
    update_rrd(),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    error_logger:info_report({rolf_recorder, handle_cast, Msg}),
    {noreply, State}.

handle_info(Info, State) ->
    error_logger:info_report({rolf_recorder, handle_info, Info}),
    {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ===================================================================
%% RRD management
%% ===================================================================

%% @doc Ensure data dir and RRD file for Service on Node exist.
ensure_rrd(ErrdServer, NodeName, ServiceName, MetricName, Type) ->
    ServiceDir = filename:join([?RRD_DIR, NodeName, ServiceName]),
    case filelib:ensure_dir(ServiceDir) of
        {error, Reason} -> {error, Reason};
        ok ->
            Filename = rrd_filename(ServiceDir, MetricName),
            case filelib:is_file(Filename) of
                false -> create_rrd(ErrdServer, Filename, MetricName, Type);
                true -> ok
            end
    end.

%% @doc Create name for RRD file for Service running on Node.
rrd_filename(ServiceDir, MetricName) ->
    filename:join(ServiceDir, string:join(MetricName, ?RRD_EXT)).

%% @doc Create an RRD file using errd_server.
create_rrd(ErrdServer, Filename, Name, Type) ->
    Cmd = errd_command:create(Filename, Name, Type),
    errd_server:command(ErrdServer, Cmd),
    ok.

%% @doc Update an RRD file with a new sample.
update_rrd() ->
    ok.

%% ===================================================================
%% Utility functions
%% ===================================================================

%% @doc Subscribe to sample messages from Server.
subscribe(Node) ->
    error_logger:info_report({rolf_recorder, subscribe, Node}),
    rpc:call(Node, rolf_server, start, []),
    rpc:call(Node, rolf_server, subscribe, [self()]).
