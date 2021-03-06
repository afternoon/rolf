%% @doc Plugin module for retrieving measurements from a munin node.
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

-module(rolf_munin_node).
-behaviour(rolf_collector).

%% rolf_collector callbacks
-export([start/1, collect/2, stop/2]).

-include("rolf.hrl").

%% ===================================================================
%% rolf_collector callbacks
%% ===================================================================

%% @doc Start collector.
start(Service) ->
    munin_node_params(Service).

%% @doc HTTP load time collector function for Rolf. Options should contain a key
%% urls with value [{Name, Url}].
collect(Service, {Host, Port, Opts, Plugin}=State) ->
    case gen_tcp:connect(Host, Port, Opts) of
        {ok, Sock} ->
            case gen_tcp:recv(Sock, 0) of
                {ok, _Banner} ->
                    Values = fetch(Sock, Plugin),
                    ok = gen_tcp:close(Sock),
                    {State, #sample{node=node(), service=Service, values=Values}};
                {error, Reason} ->
                    log4erl:error("Error reading from ~p:~p: ~p", [Host, Port, Reason]),
                    {State, #sample{node=node(), service=Service, values=[]}}
            end;
        {error, Reason} ->
            log4erl:error("Couldn't connect to ~p:~p: ~p", [Host, Port, Reason]),
            {State, #sample{node=node(), service=Service, values=[]}}
    end.

%% @doc Stop collector.
stop(_State, _Service) ->
    ok.

%% ===================================================================
%% Utility functions
%% ===================================================================

%% @doc Extract the socket parameters from service config.
munin_node_params(Service) ->
    Config = Service#service.config,
    Host = proplists:get_value(host, Config),
    Port = proplists:get_value(port, Config, 4949),
    {ok, Opts} = application:get_env(munin_node_sock_opts),
    Plugin = proplists:get_value(plugin, Config),
    {Host, Port, Opts, Plugin}.

%% @doc Fetch values from socket connection to Munin node.
fetch(Sock, Plugin) ->
    ok = gen_tcp:send(Sock, rolf_util:string_format("fetch ~p\n", [Plugin])),
    read_values(Sock).

%% @doc Read values from Sock until . on it's own on a line is encountered.
read_values(Sock) ->
    read_values(Sock, []).
read_values(Sock, Values) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, Data} ->
            case Data of
                ".\n" ->
                    log4erl:debug("Munin node fetch complete: ~p", [Values]),
                    Values;
                Line ->
                    log4erl:debug("Munin node fetch got: ~p", [Line]),
                    read_values(Sock, [parse_line(Line)|Values])
            end;
        {error, closed} ->
            Values
    end.

%% @doc Parse a line returned by Munin node. E.g. "blah.value 99\n"
parse_line(Line) ->
    K = list_to_atom(hd(string:tokens(Line, "."))),
    case rolf_util:list_to_num(hd(tl(string:tokens(Line, " ")))) of
        error -> {K, undefined};
        S -> {K, S}
    end.

%% ===================================================================
%% Tests
%% ===================================================================

-ifdef(TEST).

parse_line_test() ->
    ?assertEqual({blah, 99}, parse_line("blah.value 99\n")),
    ?assertEqual({blah, undefined}, parse_line("blah.value U\n")).

-endif.
