%% @doc Plugin module for generic commands.
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

-module(rolf_command).
-behaviour(rolf_collector).

%% rolf_collector callbacks
-export([start/1, collect/2, stop/2]).

-include("rolf.hrl").

%% ===================================================================
%% rolf_collector callbacks
%% ===================================================================

%% @doc Start collector.
start(_Service) -> ok.

%% @doc Execute the command and return the parsed results.
collect(Service, State) ->
    {State, parse_output(Service, os:cmd(Service#service.command))}.

%% @doc Stop collector.
stop(_Service, _State) -> ok.

%% ===================================================================
%% Helper functions
%% ===================================================================

%% @doc Parse output from external command.
parse_output(Service, Output) ->
    Values = [parse_line(Line) || Line <- split_lines(Output)],
    #sample{node=node(), service=Service, values=Values}.

%% @doc Split output into lines, drop terminating ".\n" line.
split_lines(Lines) ->
    Lines1 = string:tokens(Lines, "\n"),
    lists:filter(fun(L) -> L /= "." end, Lines1).

%% @doc Parse line into {atom, int_or_float} tuple.
parse_line(Line) ->
    {K, V} = list_to_tuple(string:tokens(Line, " ")),
    {list_to_atom(K), rolf_util:list_to_num(V)}.

%% ===================================================================
%% Tests
%% ===================================================================

-ifdef(TEST).

split_lines_test() ->
    Result = split_lines("loadtime 0.99\n.\n"),
    ?assertEqual(["loadtime 0.99"], Result).

parse_line_test() ->
    Result = parse_line("loadtime 0.99"),
    ?assertEqual({loadtime, 0.99}, Result).

parse_output_test() ->
    Result = parse_output(loadtime, "loadtime 0.99\n.\n"),
    ?assertEqual(#sample{node=node(),
                         service=loadtime,
                         values=[{loadtime, 0.99}]},
                 Result).

parse_output_many_test() ->
    Result = parse_output(loadtime, "loadtime 0.99\nttfb 0.65\nrendertime 2\n.\n"),
    ?assertEqual(#sample{node=node(),
                         service=loadtime,
                         values=[{loadtime, 0.99},
                                 {ttfb, 0.65},
                                 {rendertime, 2}]},
                 Result).

-endif.
