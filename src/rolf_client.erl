%% @doc Client which listens for measurements from services and writes them
%% down.
%% @author Ben Godfrey <ben@ben2.com> [http://aftnn.org/]
%% @copyright 2011 Ben Godfrey
%% @version 1.0.0

-module(rolf_client).
-export([subscribe/1, update/1]).

%% @doc Subscribe to measurement messages from Server.
subscribe(Node) ->
    io:format("rolf_client:subscribe~n"),
    rpc:call(Node, rolf_server, start, []),
    rpc:call(Node, rolf_server, subscribe, []).

%% @doc Handle measurement message.
update(Results) ->
    io:format("Result: ~p~n", [Results]).
