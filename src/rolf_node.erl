%% @doc Node which is monitored.
%% @author Ben Godfrey <ben@ben2.com> [http://aftnn.org/]
%% @copyright 2011 Ben Godfrey
%% @version 1.0.0

-module(rolf_node).
-behaviour(gen_server).
-export([start/0, stop/0, list/0, fetch/1, config/1,
        start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

%% data

-record(nodestate, {services=[]}).

-define(PLUGIN_DIR, "../plugins").

%% interface

start() ->          gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() ->           gen_server:call(?MODULE, stop).
list() ->           gen_server:call(?MODULE, list).
fetch(Service) ->   gen_server:call(?MODULE, {fetch, Service}).
config(Service) ->  gen_server:call(?MODULE, {config, Service}).

%% gen_server implementation

init([]) ->
    {ok, #nodestate{services=[disk]}}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

handle_call(stop, _From, NodeState) ->
    {stop, normal, stopped, NodeState};

handle_call(list, _From, NodeState) ->
    {reply, NodeState#nodestate.services, NodeState};

handle_call(_Request, _From, NodeState) ->
    {reply, ok, NodeState}.

handle_cast(_Msg, Ref) ->
    {noreply, Ref}.

handle_info(_Info, Ref) ->
    {noreply, Ref}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, NodeState, _Extra) ->
    {ok, NodeState}.
