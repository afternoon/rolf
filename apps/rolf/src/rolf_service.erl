%% @doc gen_server which provides monitoring information for a single service on
%% a machine.
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

-module(rolf_service).

-behaviour(gen_server).

%% API
-export([start_link/1, stop/1, publish/1, start_emitting/1, stop_emitting/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-include("rolf.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc Start a service using Service as initial state.
start_link(Service) ->
    gen_server:start_link({local, server_name(Service)}, ?MODULE, [Service], []).

%% @doc Stop service Name.
stop(Name) ->
    gen_server:call(server_name(Name), stop).

%% @doc Start emitting samples. Emit one straight away and then set a timer to
%% emit regularly.
start_emitting(Name) ->
    gen_server:cast(server_name(Name), start_emitting).

%% @doc Stop emitting samples.
stop_emitting(Name) ->
    gen_server:cast(server_name(Name), stop_emitting).

%% @doc Trigger polling of this service manually, useful for inspecting and debugging
publish(Name) ->
    gen_server:cast(server_name(Name), publish).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

%% @doc Start service, create a timer which will sample for results regularly and
%% publish them to the recorder.
init([Service]) ->
    process_flag(trap_exit, true),
    net_kernel:monitor_nodes(true),
    CState = apply(Service#service.module, start, [Service]),
    {ok, {Service, CState}}.

%% @doc Log unhandled calls.
handle_call(Req, From, State) ->
    log4erl:debug("Unhandled call from ~p: ~p", [From, Req]),
    {reply, State}.

handle_cast(start_emitting, {Service, CState}) ->
    Name = Service#service.name,
    Freq = Service#service.frequency,
    log4erl:info("~p started emitting (frequency ~p)", [Name, Freq]),
    apply(?MODULE, publish, [Name]),
    case timer:apply_interval(timer:seconds(Freq), ?MODULE, publish, [Name]) of
        {ok, TRef} ->
            {noreply, {Service#service{tref=TRef}, CState}};
        _ ->
            {noreply, {Service, CState}}
    end;

handle_cast(stop_emitting, {Service, CState}) ->
    log4erl:info("~p stopped emitting", [Service#service.name]),
    timer:cancel(Service#service.tref),
    {noreply, {#service{tref=undefined}, CState}};

handle_cast(publish, {Service, CState}) ->
    Sample = apply(Service#service.module, collect, [Service, CState]),
    send(Service#service.recorders, Sample),
    {noreply, {Service, CState}};

%% @doc Log unhandled casts.
handle_cast(Req, State) ->
    log4erl:debug("Unhandled cast: ~p", [Req]),
    {noreply, State}.

%% @doc Handle nodeup messages from monitoring nodes.
handle_info({nodeup, Node}, {Service, CState}) ->
    case lists:member(Node, rolf_recorder:recorders()) of
        true ->
            log4erl:info("Recorder ~p up", [Node]),
            OldRecs = Service#service.recorders,
            {noreply, {Service#service{recorders=[Node|OldRecs]}, CState}};
        _ ->
            {noreply, {Service, CState}}
    end;

%% @doc Handle nodedown messages from monitoring nodes.
handle_info({nodedown, Node}, {Service, CState}) ->
    Live = rolf_recorder:live_recorders(),
    case Live of
        [] ->
            log4erl:info("Recorder ~p down, exiting", [Node]),
            {stop, no_recorders, {Service, CState}};
        _ ->
            log4erl:info("Recorder ~p down", [Node]),
            {noreply, Service#service{recorders=Live}}
    end;

%% @doc Log unhandled info messages.
handle_info(Info, State) ->
    log4erl:debug("Unhandled info: ~p", [Info]),
    {noreply, State}.

%% @doc Terminate
terminate(_Reason, {Service, CState}) ->
    stop_emitting(Service),
    Module = Service#service.module,
    apply(Module, stop, [Service, CState]),
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ===================================================================
%% Utility functions
%% ===================================================================

%% @doc Get canonical name of service from name atom or service record.
server_name(Name) when is_atom(Name) ->
    list_to_atom(string:join([atom_to_list(A) || A <- [?MODULE, Name]], "_"));
server_name(#service{name=Name}) ->
    server_name(Name).

%% @doc Send sample to all live recorders.
send(Recorders, Sample) ->
    lists:foreach(fun(R) -> rpc:call(R, rolf_recorder, store, [Sample]) end,
        Recorders).

%% ===================================================================
%% Tests
%% ===================================================================

-ifdef(TEST).

server_name_test() ->
    Name = list_to_atom("rolf_service_loadtime"),
    ?assertEqual(Name, server_name(loadtime)),
    ?assertEqual(Name, server_name(#service{name=loadtime})).

-endif.
