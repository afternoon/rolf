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
-export([start/0, stop/0, subscribe/1, unsubscribe/1, start_link/0, init/1,
        handle_call/3, handle_cast/2, handle_info/2, terminate/2,
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

%% ===================================================================
%% Server callbacks
%% ===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    io:format("rolf_server:init"),
    Services = find_services(),
    start_services(Services),
    {ok, #server{clients=[], services=Services}}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State}.

handle_cast({subscribe, C}, #server{clients=Clients, services=Services} = State) ->
    io:format("rolf_server:subscribe"),
    lists:foreach(fun(S) -> rolf_service:subscribe(S#service.name, C) end, Services),
    {reply, ok, State#server{clients=[C|Clients]}};

handle_cast({unsubscribe, C}, #server{clients=Clients} = State) ->
    {reply, ok, State#server{clients=lists:delete(C, Clients)}}.

handle_info(_Info, Ref) ->
    {noreply, Ref}.

terminate(_Reason, #server{services=Services}) ->
    lists:foreach(fun(S) -> rolf_service:stop(S#service.name) end, Services),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Utility functions
%% ===================================================================

%% @doc List all services configured to run on this server.
find_services() ->
    io:format("rolf_server:find_services"),
    Name = loadtime,
    Cmd = [rolf_service, invoke, loadtime],
    Freq = 5000,
    [#service{name=Name, cmd=Cmd, freq=Freq, clients=[], tref=undef}].

%% @doc Start an instance of the rolf_service gen_server for each service.
start_services([S|Services]) ->
    io:format("rolf_server:start_services"),
    rolf_service:start(S),
    start_services(Services).
