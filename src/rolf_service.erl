%% @doc gen_server which provides monitoring information for a single service on
%% a machine.
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

-define(PLUGIN_DIR, filename:join("priv", "plugin.d")).

%% ===================================================================
%% API
%% ===================================================================

%% @doc Start a service using Service as initial state.
start(Service) -> gen_server:start_link({local, service_name(Service)}, ?MODULE, [Service], []).

%% @doc Stop service Name.
stop(Name) -> gen_server:call(service_name(Name), stop).

%% @doc Subscribe Recorder to updates from this service.
subscribe(Name, Recorder) -> gen_server:cast(service_name(Name), {subscribe, Recorder}).

%% @doc Unsubscribe Recorder from updates from this service.
unsubscribe(Name, Recorder) -> gen_server:cast(service_name(Name), {unsubscribe, Recorder}).

%% @doc Trigger polling of this service manually, useful for inspecting and debugging
publish(Name) -> gen_server:cast(service_name(Name), {publish}).

%% @doc Get internal state - for debugging.
get_state(Name) -> gen_server:call(service_name(Name), get_state).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

%% @doc Start service, create a timer which will sample for results regularly and
%% publish them to recorders.
init([Service]) ->
    error_logger:info_report({rolf_service, init, Service}),
    start_emitting(Service),
    {ok, Service}.

handle_call(get_state, _From, Service) ->
    {reply, Service, Service};

handle_call(stop, _From, Service) ->
    error_logger:info_report({rolf_service, stop}),
    {stop, normal, stopped, Service}.

handle_cast({subscribe, R}, #service{recorders=Rs} = Service) ->
    error_logger:info_report({rolf_service, subscribe, R}),
    case (lists:member(R, Rs)) of
        true -> {noreply, Service};
        false -> {noreply, Service#service{recorders=[R|Rs]}}
    end;

handle_cast({unsubscribe, R}, #service{recorders=Rs} = Service) ->
    error_logger:info_report({rolf_service, unsubscribe, R}),
    {noreply, Service#service{recorders=lists:delete(R, Rs)}};

handle_cast({publish}, Service) ->
    Recorders = Service#service.recorders,
    error_logger:info_report({rolf_service, publish, Recorders}),
    case (Recorders) of
        [] ->
            ok;
        _ -> 
            [M, F, A] = Service#service.cmd,
            Samples = apply(M, F, A),
            lists:foreach(fun(R) -> send(R, Samples) end, Recorders)
    end,
    {noreply, Service}.

handle_info(Info, Ref) ->
    error_logger:info_report({rolf_service, handle_info, Info}),
    {noreply, Ref}.

terminate(_Reason, Service) -> stop_emitting(Service).

code_change(_OldVsn, Service, _Extra) -> {ok, Service}.

%% ===================================================================
%% Utility functions
%% ===================================================================

%% @doc Get canonical name of service from name atom or service record.
service_name(Name) when is_atom(Name) ->
    list_to_atom(string:join([?MODULE, atom_to_list(Name)], "_"));
service_name(Service) ->
    service_name(Service#service.name).

%% @doc Send a set of samples to a client.
send(_Client, Samples) ->
    error_logger:info_report({rolf_service, send, Samples}),
    rolf_recorder:store(Samples).

%% @doc Invoke plug-in and return samples.
invoke(Plugin) ->
    error_logger:info_report({rolf_service, invoke, Plugin}),
    Prog = filename:join(?PLUGIN_DIR, atom_to_list(Plugin)),
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
