%% @doc Server which provides monitoring information for a machine.
%% @author Ben Godfrey <ben@ben2.com> [http://aftnn.org/]
%% @copyright 2011 Ben Godfrey
%% @version 1.0.0

-module(rolf_server).
-behaviour(gen_server).
-export([start/0, stop/0, add_service/1, subscribe/0, start_link/0, init/1,
        handle_call/3, handle_cast/2, handle_info/2, terminate/2,
        code_change/3]).

%% data

-define(PLUGIN_DIR, "../plugin.d").

-record(server, {clients, services}).

%% interface

start() ->              gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() ->               gen_server:call(?MODULE, stop).
add_service(Service) -> gen_server:call(?MODULE, {add_service, Service}).
subscribe() ->          gen_server:call(?MODULE, subscribe).

%% gen_server implementation

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    io:format("rolf_server:init"),
    State = #server{services=[]},
    {ok, State}.

handle_call(subscribe, {Pid, _Tag}, #server{clients=Clients, services=Services} = State) ->
    io:format("rolf_server:subscribe"),
    lists:foreach(fun(S) -> S ! {subscribe, Pid} end, Services),
    {reply, ok, State#server{clients=[Pid|Clients]}};

handle_call({add_service, S}, _From, #server{clients=Clients, services=Services} = State) ->
    io:format("rolf_server:add_service"),
    lists:foreach(fun(C) -> rolf_service:subscribe(C) end, Clients),
    {reply, ok, State#server{services=[S|Services]}};

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State}.

handle_cast(_Msg, Ref) ->
    {noreply, Ref}.

handle_info(_Info, Ref) ->
    {noreply, Ref}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
