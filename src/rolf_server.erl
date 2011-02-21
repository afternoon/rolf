%% @doc Server which provides monitoring information for a machine.
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

-module(rolf_server).
-behaviour(gen_server).
-export([
        % api
        start/0, stop/0, subscribe/1, unsubscribe/1, get_state/0,
        % gen_server
        init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
        code_change/3]).
-include("rolf.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc Start a rolf server on this node, start all services.
start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Stop server and services.
stop() -> gen_server:call(?MODULE, stop).

%% @doc Add a subscription for Client to receive all updates.
subscribe(Client) -> gen_server:cast(?MODULE, {subscribe, Client}).

%% @doc Remove subscription for client.
unsubscribe(Client) -> gen_server:cast(?MODULE, {unsubscribe, Client}).

%% @doc Get internal state - for debugging.
get_state() -> gen_server:call(?MODULE, get_state).

%% ===================================================================
%% Server callbacks
%% ===================================================================

init([]) ->
    error_logger:info_report({rolf_server, init}),
    Services = find_services(),
    start_services(Services),
    {ok, #server{clients=[], services=Services}}.

handle_call(get_state, _From, State) ->
    error_logger:info_report({rolf_server, get_state, State}),
    {reply, State, State};

handle_call(stop, _From, #server{services=Services} = State) ->
    error_logger:info_report({rolf_server, stop}),
    lists:foreach(fun(S) -> rolf_service:stop(S#service.name) end, Services),
    {stop, normal, stopped, State}.

handle_cast({subscribe, C}, #server{clients=Clients, services=Services} = State) ->
    error_logger:info_report({rolf_server, subscribe, C}),
    lists:foreach(fun(S) -> rolf_service:subscribe(S#service.name, C) end, Services),
    {noreply, State#server{clients=[C|Clients]}};

handle_cast({unsubscribe, C}, #server{clients=Clients, services=Services} = State) ->
    error_logger:info_report({rolf_server, unsubscribe, C}),
    lists:foreach(fun(S) -> rolf_service:unsubscribe(S#service.name, C) end, Services),
    {noreply, State#server{clients=lists:delete(C, Clients)}}.

handle_info(_Info, Ref) ->
    {noreply, Ref}.

terminate(_Reason, #server{services=Services}) ->
    error_logger:info_report({rolf_server, terminate}),
    lists:foreach(fun(S) -> rolf_service:stop(S#service.name) end, Services),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Utility functions
%% ===================================================================

%% @doc List all services configured to run on this server.
find_services() ->
    error_logger:info_report({rolf_server, find_services}),
    Name = loadtime,
    Cmd = [rolf_service, invoke, [loadtime]],
    Freq = 5000,
    Services = [#service{name=Name, cmd=Cmd, freq=Freq, clients=[],
            tref=undef}],
    error_logger:info_report({rolf_server, find_services, Services}),
    Services.

%% @doc Start an instance of the rolf_service gen_server for each service.
start_services([]) -> ok;
start_services([S|Services]) ->
    error_logger:info_report({rolf_server, start_services, S}),
    rolf_service:start(S),
    start_services(Services).
