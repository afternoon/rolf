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
-export([start_link/0, stop/0, ensure_rrd/2, store/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-include("rolf.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc Start a recorder on this node.
start_link() -> gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

%% @doc Stop recorder.
stop() -> gen_server:call(?MODULE, stop).

%% @doc Ensure .rrd file exists for Service on Node.
ensure_rrd(Node, Service) ->
    gen_server:call(?MODULE, {ensure_rrd, Node, Service}).

%% @doc Pass samples to all recorders in the cluster.
store(Sample) -> gen_server:cast(?MODULE, {store, Sample}).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    process_flag(trap_exit, true),
    error_logger:info_report({rolf_recorder, init}),

    % start errd_server
    case errd_server:start_link() of
        {ok, RRD} ->
            {ok, #recorder{rrd=RRD}};
        Else ->
            error_logger:error_report({rolf_recorder, errd_server, error, Else}),
            {stop, Else}
    end.

handle_call({ensure_rrd, Node, Service}, _From, #recorder{rrd=RRD}=State) ->
    Reply = rolf_rrd:ensure(RRD, Node, Service),
    {reply, Reply, State}.

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

terminate(Reason, #recorder{rrd=RRD}) ->
    error_logger:info_report({rolf_recorder, terminate, Reason}),
    errd_server:stop(RRD),
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
