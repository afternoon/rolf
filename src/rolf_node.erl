%% @doc gen_server which manages monitoring services running on an Erlang node.
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

-module(rolf_node).
-behaviour(gen_server).
-export([
        % api
        start_link/0, stop/0, get_state/0,
        % gen_server
        init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
        code_change/3]).

-include("rolf.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc Start a rolf node on this node, start all services.
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Stop node and services.
stop() -> gen_server:call(?MODULE, stop).

%% @doc Get internal state - for debugging.
get_state() -> gen_server:call(?MODULE, get_state).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    error_logger:info_report({rolf_node, node(), init}),
    Services = find_services(),
    start_services(Services),
    {ok, #node{services=Services}}.

handle_call(get_state, _From, State) ->
    error_logger:info_report({rolf_node, node(), get_state, State}),
    {reply, State, State};

handle_call(stop, _From, #node{services=Services}=State) ->
    error_logger:info_report({rolf_node, node(), stop}),
    lists:foreach(fun(S) -> rolf_service:stop(S#service.name) end, Services),
    {stop, normal, stopped, State}.

handle_cast(Msg, State) ->
    error_logger:info_report({rolf_recorder, handle_cast, Msg}),
    {noreply, State}.

handle_info(Info, State) ->
    error_logger:info_report({rolf_node, node(), handle_info, Info}),
    {noreply, State}.

terminate(_Reason, #node{services=Services}) ->
    error_logger:info_report({rolf_node, node(), terminate}),
    lists:foreach(fun(S) -> rolf_service:stop(S#service.name) end, Services),
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ===================================================================
%% Utility functions
%% ===================================================================

%% @doc List all services configured to run on this node.
find_services() ->
    error_logger:info_report({rolf_node, node(), find_services}),
    Freq = 10000,
    [#service{
        name=loadtime,
        cmd=[rolf_service, invoke, [loadtime]],
        freq=Freq},
     #service{
        name=disk,
        cmd=[rolf_service, invoke, [disk]],
        freq=Freq}].

%% @doc Start an instance of the rolf_service gen_server for each service.
start_services([]) -> ok;
start_services([S|Services]) ->
    error_logger:info_report({rolf_node, node(), start_services, S}),
    Result = rolf_service:start_link(S),
    error_logger:info_report({rolf_node, node(), start_services, Result}),
    start_services(Services).
