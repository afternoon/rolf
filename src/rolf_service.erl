%% @doc Server which provides monitoring information for a single service on a
%% machine.
%% @author Ben Godfrey <ben@ben2.com> [http://aftnn.org/]
%% @copyright 2011 Ben Godfrey
%% @version 1.0.0

-module(rolf_service).
-behaviour(gen_server).
-export([start/1, stop/1, subscribe/2, poll/1, start_link/0, init/1,
        handle_call/3, handle_cast/2, handle_info/2, terminate/2,
        code_change/3, dummy/0, invoke/1]).

%% data

-define(PLUGIN_DIR, "../plugin.d").

-record(service, {name, cmd, freq, clients, tref}).

%% interface

start(Name) ->         gen_server:start_link({local, Name}, ?MODULE, [Name], []).
stop(Name) ->          gen_server:call(Name, stop).
subscribe(Name, C) ->  gen_server:call(Name, {subscribe, C}).
poll(Name) ->          gen_server:call(Name, {poll}).

%% gen_server implementation

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Start service, create a timer which will poll for results regularly and
%% send them to any clients.
init([]) ->
    io:format("rolf_service:init"),
    Freq = 5000,
    case timer:apply_interval(Freq, ?MODULE, poll, []) of
        {ok, TimerRef} ->
            Service = dummy(),
            {ok, Service#service{tref=TimerRef}};
        _              -> error
    end.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call({subscribe, C}, _From, #service{clients=Clients} = State) ->
    io:format("rolf_service:subscribe"),
    {reply, ok, State#service{clients=[C|Clients]}};

handle_call({poll}, _From, State) ->
    io:format("rolf_service:poll"),
    Clients = State#service.clients,
    case (Clients) of
        [] ->
            ok;
        _ -> 
            [M, F, A] = State#service.cmd,
            Results = apply(M, F, A),
            lists:foreach(fun(C) -> send(C, Results) end, Clients)
    end,
    {reply, ok, State}.

handle_cast(_Msg, Ref) ->
    {noreply, Ref}.

handle_info(_Info, Ref) ->
    {noreply, Ref}.

terminate(_Reason, #service{tref=TimerRef}) ->
    timer:cancel(TimerRef),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% util functions

dummy() ->
    Name = loadtime,
    Cmd = [?MODULE, invoke, loadtime],
    Freq = 5000,
    #service{name=Name, cmd=Cmd, freq=Freq, clients=[], tref=undef}.

%% @doc Send a set of results to a client.
send(Client, Results) ->
    rpc:call(Client, rolf_client, update, [Results]).

%% @doc Invoke plug-in and return results.
invoke(Plugin) ->
    Prog = lists:concat([?PLUGIN_DIR, atom_to_list(Plugin)]),
    os:cmd(Prog).
