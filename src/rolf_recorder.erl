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
-export([
        %api
        start_link/0, stop/0, store/1,
        % gen_server
        init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
        code_change/3]).

-include("rolf.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc Start a recorder on this node.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Stop recorder.
stop() -> gen_server:call(?MODULE, stop).

%% @doc Pass samples to all recorders in the cluster.
store(Samples) -> gen_server:abcast(?MODULE, {store, Samples}).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    error_logger:info_report({rolf_recorder, init}),

    % start errd server and rolf_node servers on the cluster
    % TODO supervisor should do these bits?
    case errd_server:start_link() of
        {ok, RRD} ->
            Nodes = [node()|nodes(connected)],
            error_logger:info_report({rolf_recorder, nodes, Nodes}),
            rpc:multicall(Nodes, rolf_node, start_link, []),
            {ok, #recorder{rrd=RRD}};
        {stop, Reason} ->
            error_logger:error_report({rolf_recorder, Reason})
    end.


handle_call(get_state, _From, State) ->
    error_logger:info_report({rolf_recorder, get_state, State}),
    {reply, State, State}.

handle_cast({store, Samples}, #recorder{rrd=RRD}=State) ->
    error_logger:info_report({rolf_recorder, store, Samples}),
    lists:foreach(fun(S) -> rolf_rrd:update(RRD, S) end, Samples),
    {noreply, State};

handle_cast(Msg, State) ->
    error_logger:info_report({rolf_recorder, handle_cast, Msg}),
    {noreply, State}.

handle_info(Info, State) ->
    error_logger:info_report({rolf_recorder, handle_info, Info}),
    {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
