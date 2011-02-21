%% @doc Server which provides monitoring information for a single service on a
%% machine.
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

-module(rolf_service).
-behaviour(gen_server).
-export([
        % api
        start/1, stop/1, subscribe/2, unsubscribe/2, publish/1, get_state/1,
        % gen_server
        init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
        code_change/3,
        % utils
        invoke/1, start_emitting/1, stop_emitting/1]).
-include("rolf.hrl").

-define(PLUGIN_DIR, "plugin.d").

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc Start a service using Service as initial state.
start(Service) -> gen_server:start_link({local, service_name(Service)}, ?MODULE, [Service], []).

%% @doc Stop service Name.
stop(Name) -> gen_server:call(service_name(Name), stop).

%% @doc Subscribe Client to updates from this service.
subscribe(Name, Client) -> gen_server:cast(service_name(Name), {subscribe, Client}).

%% @doc Unsubscribe Client from updates from this service.
unsubscribe(Name, Client) -> gen_server:cast(service_name(Name), {unsubscribe, Client}).

%% @doc Trigger polling of this service manually, useful for inspecting and debugging
publish(Name) -> gen_server:cast(service_name(Name), {publish}).

%% @doc Get internal state - for debugging.
get_state(Name) -> gen_server:call(service_name(Name), get_state).

%% ===================================================================
%% Server callbacks
%% ===================================================================

%% @doc Start service, create a timer which will poll for results regularly and
%% publish them to clients.
init([Service]) ->
    error_logger:info_report({rolf_service, init, Service}),
    start_emitting(Service),
    {ok, Service}.

handle_call(get_state, _From, Service) ->
    {reply, Service, Service};

handle_call(stop, _From, Service) ->
    error_logger:info_report({rolf_service, stop}),
    {stop, normal, stopped, Service}.

handle_cast({subscribe, C}, #service{clients=Clients} = Service) ->
    error_logger:info_report({rolf_service, subscribe, C}),
    case (lists:member(C, Clients)) of
        true -> {noreply, Service};
        false -> {noreply, Service#service{clients=[C|Clients]}}
    end;

handle_cast({unsubscribe, C}, #service{clients=Clients} = Service) ->
    error_logger:info_report({rolf_service, unsubscribe, C}),
    {noreply, Service#service{clients=lists:delete(C, Clients)}};

handle_cast({publish}, Service) ->
    Clients = Service#service.clients,
    error_logger:info_report({rolf_service, publish, Clients}),
    case (Clients) of
        [] ->
            ok;
        _ -> 
            [M, F, A] = Service#service.cmd,
            Results = apply(M, F, A),
            lists:foreach(fun(C) -> send(C, Results) end, Clients)
    end,
    {noreply, Service}.

handle_info(_Info, Ref) ->
    {noreply, Ref}.

terminate(_Reason, Service) ->
    stop_emitting(Service).

code_change(_OldVsn, Service, _Extra) ->
    {ok, Service}.

%% ===================================================================
%% Utility functions
%% ===================================================================

%% @doc Get canonical name of service from name atom or service record.
service_name(Name) when is_atom(Name) ->
    list_to_atom(lists:concat([?MODULE, "_", atom_to_list(Name)]));
service_name(Service) ->
    service_name(Service#service.name).

%% @doc Send a set of results to a client.
send(Client, Results) ->
    error_logger:info_report({rolf_service, send, Client, Results}),
    Client ! {results, Results}.

%% @doc Invoke plug-in and return results.
invoke(Plugin) ->
    error_logger:info_report({rolf_service, invoke, Plugin}),
    Prog = lists:concat([?PLUGIN_DIR, "/", atom_to_list(Plugin)]),
    os:cmd(Prog).

start_emitting(Service) ->
    error_logger:info_report({rolf_service, start_emitting}),
    Name = Service#service.name,
    Freq = Service#service.freq,
    case timer:apply_interval(Freq, ?MODULE, publish, [Name]) of
        {ok, TRef} ->
            {ok, Service#service{tref=TRef}};
        _ ->
            error
    end.

stop_emitting(Service) ->
    error_logger:info_report({rolf_service, stop_emitting}),
    timer:cancel(Service#service.tref).
