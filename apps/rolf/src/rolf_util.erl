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

-module(rolf_util).

%% rolf_collector callbacks
-export([string_format/2, list_to_num/1, strip_whitespace/1]).

-include("rolf.hrl").

%% ===================================================================
%% Utility functions
%% ===================================================================

%% @doc Sane string formatting.
string_format(Pattern, Values) ->
    lists:flatten(io_lib:format(Pattern, Values)).

%% @doc Coerce a string to a float or an integer.
list_to_num(S) ->
    S_ = strip_whitespace(S),
    try list_to_float(S_) catch
        error:badarg ->
            try list_to_integer(S_) catch
                error:badarg -> error
            end
    end.

%% @doc Strip surrounding whitespace from a string.
strip_whitespace(S) ->
    strip_whitespace(S, []).
strip_whitespace([C|Cs], S) when C == $\s; C == $\n; C == $\r; C == $\t ->
    S ++ strip_whitespace(Cs);
strip_whitespace([C|Cs], S) ->
    S ++ [C|strip_whitespace(Cs)];
strip_whitespace([], S) ->
    S.

%% ===================================================================
%% Tests
%% ===================================================================

-ifdef(TEST).

string_format_test() ->
    ?assertEqual("X:1:9.5:z", string_format("~s:~b:~.1f:~p", ["X", 1, 9.5, z])).

list_to_num_test() ->
    ?assertEqual(99, list_to_num("99")),
    ?assertEqual(-1, list_to_num("-1")),
    ?assertEqual(0.999, list_to_num("0.999")),
    ?assertEqual(-3.14, list_to_num("-3.14")),
    ?assertEqual(99, list_to_num("  99\n ")),
    ?assertEqual(error, list_to_num("monkey")).

strip_whitespace_test() ->
    ?assertEqual("blah", strip_whitespace("  blah\n\t ")).
-endif.
