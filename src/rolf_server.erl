%% @doc Server which provides monitoring information for a machine.
%% @author Ben Godfrey <ben@ben2.com> [http://aftnn.org/]
%% @copyright 2011 Ben Godfrey
%% @version 1.0.0

-module(rolf_server).
-behaviour(gen_server).
-export([start/0, stop/0, subscribe/1, unsubscribe/1, start_link/0, init/1,
        handle_call/3, handle_cast/2, handle_info/2, terminate/2,
        code_change/3]).
-include("rolf.hrl").

%% interface

%% @doc Start a rolf server on this node, start all services.
start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Stop server and services.
stop() -> gen_server:call(?MODULE, stop).

%% @doc Add a subscription for Client to receive all updates.
subscribe(Client) -> gen_server:call(?MODULE, {subscribe, Client}).

%% @doc Remove subscription for client.
unsubscribe(Client) -> gen_server:call(?MODULE, {unsubscribe, Client}).

%% gen_server implementation

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    io:format("rolf_server:init"),
    Services = find_services(),
    start_services(Services),
    {ok, #server{clients=[], services=Services}}.

handle_call({subscribe, C}, _From, #server{clients=Clients, services=Services} = State) ->
    io:format("rolf_server:subscribe"),
    lists:foreach(fun(S) -> rolf_service:subscribe(S#service.name, C) end, Services),
    {reply, ok, State#server{clients=[C|Clients]}};

handle_call({unsubscribe, C}, _From, #server{clients=Clients} = State) ->
    {reply, ok, State#server{clients=lists:delete(C, Clients)}};

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State}.

handle_cast(_Msg, Ref) ->
    {noreply, Ref}.

handle_info(_Info, Ref) ->
    {noreply, Ref}.

terminate(_Reason, #server{services=Services}) ->
    lists:foreach(fun(S) -> rolf_service:stop(S#service.name) end, Services),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% utils

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
