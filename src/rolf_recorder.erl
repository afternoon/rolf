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
        start/0, stop/0, store/1,
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

%% @doc Start a recorder on this node.
start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Stop recorder.
stop() -> gen_server:call(?MODULE, stop).

%% @doc Pass samples to all recorders in the cluster.
store(Samples) -> gen_server:abcast(?MODULE, {store, Samples}).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    error_logger:info_report({rolf_recorder, init}),
    ErrdServer = errd_server:start_link(), % supervisor should do this?
    {ok, #recorder{errdserver=ErrdServer}}.

handle_call(get_state, _From, State) ->
    error_logger:info_report({rolf_recorder, get_state, State}),
    {reply, State, State}.

handle_cast({store, Samples}, State) ->
    error_logger:info_report({rolf_recorder, store, Samples}),
    ensure_rrd(State#recorder.errdserver, 'josie@josie', foo, foos, counter),
    update_rrd(),
    {noreply, State};

handle_cast(Msg, State) ->
    error_logger:info_report({rolf_recorder, handle_cast, Msg}),
    {noreply, State}.

handle_info(Info, State) ->
    error_logger:info_report({rolf_recorder, handle_info, Info}),
    {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ===================================================================
%% Utility functions
%% ===================================================================

%% @doc Subscribe to sample messages from Server.
subscribe(Node) ->
    error_logger:info_report({rolf_recorder, subscribe, Node}),
    rpc:call(Node, rolf_server, start, []),
    rpc:call(Node, rolf_server, subscribe, [self()]).
