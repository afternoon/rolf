%% @doc Server which provides monitoring information for a single service on a
%% machine.
%% @author Ben Godfrey <ben@ben2.com> [http://aftnn.org/]
%% @copyright 2011 Ben Godfrey
%% @version 1.0.0

-module(rolf_service).
-behaviour(gen_server).
-export([
        % interface
        start/1, stop/1, subscribe/2, poll/1,
        % gen_server
        start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3,
        % utils
        invoke/1, start_emitting/1, stop_emitting/1]).
-include("rolf.hrl").

-define(PLUGIN_DIR, "../plugin.d").

%% interface

%% @doc Start a service using Service as initial state.
start(#service{name=Name}) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name], []).

%% @doc Stop service Name.
stop(Name) ->
    gen_server:call(Name, stop).

%% @doc Subscribe Client to updates from this service.
subscribe(Name, Client) ->
    gen_server:call(Name, {subscribe, Client}).

poll(Name) ->
    gen_server:call(Name, {poll}).

%% gen_server implementation

start_link() ->
    io:format("rolf_service:start_link~n"),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Start service, create a timer which will poll for results regularly and
%% publish them to clients.
init([]) ->
    io:format("rolf_service:init~n").

handle_call(stop, _From, State) ->
    io:format("rolf_service:stop~n"),
    {stop, normal, stopped, State};

handle_call({subscribe, C}, _From, #service{clients=Clients} = State) ->
    io:format("rolf_service:subscribe~n"),
    {reply, ok, State#service{clients=[C|Clients]}};

handle_call({unsubscribe, C}, _From, #service{clients=Clients} = State) ->
    io:format("rolf_service:unsubscribe~n"),
    {reply, ok, State#service{clients=lists:delete(C, Clients)}};

handle_call({poll}, _From, State) ->
    io:format("rolf_service:poll~n"),
    Clients = State#service.clients,
    case (Clients) of
        [] ->
            io:format("rolf_service:poll: no clients~n"),
            ok;
        _ -> 
            io:format("rolf_service:poll: some clients~n"),
            [M, F, A] = State#service.cmd,
            Results = apply(M, F, A),
            lists:foreach(fun(C) -> publish(C, Results) end, Clients)
    end,
    {reply, ok, State}.

handle_cast(_Msg, Ref) ->
    {noreply, Ref}.

handle_info(_Info, Ref) ->
    {noreply, Ref}.

terminate(_Reason, Service) ->
    stop_emitting(Service).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% util functions

%% @doc Send a set of results to a client.
publish(Client, Results) ->
    io:format("rolf_service:publish~n"),
    rpc:call(Client, rolf_client, update, [Results]).

%% @doc Invoke plug-in and return results.
invoke(Plugin) ->
    io:format("rolf_service:invoke~n"),
    Prog = lists:concat([?PLUGIN_DIR, atom_to_list(Plugin)]),
    os:cmd(Prog).

start_emitting(Service) ->
    io:format("rolf_service:start_emitting~n"),
    case timer:apply_interval(Service#service.freq, ?MODULE, poll, []) of
        {ok, TRef} ->
            {ok, Service#service{tref=TRef}};
        _ ->
            error
    end.

stop_emitting(Service) ->
    io:format("rolf_service:stop_emitting~n"),
    timer:cancel(Service#service.tref).
