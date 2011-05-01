%% @doc Utilities for managing RRD files with errd.
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

-module(rolf_rrd).

%% API
-export([ensure/3, update/2]).

-include_lib("errd/include/errd.hrl").
-include("rolf.hrl").

-define(RRD_DIR, "data").
-define(RRD_EXT, "rrd").

%% ===================================================================
%% API
%% ===================================================================

%% @doc Ensure data dir and RRD file for Service on Node exist.
ensure(RRD, Node, Service) ->
    Path = rrd_path(Node, Service),
    case filelib:ensure_dir(Path) of
        {error, Reason} ->
            log4erl:error("Couldn't create RRD dir ~p: ~p", [Path, Reason]),
            {error, Reason};
        ok ->
            case filelib:is_file(Path) of
                false -> create(RRD, Path, Service);
                true ->  ok
            end
    end.

%% @doc Create an RRD file for a service. Metrics should be a list of
%% {Name, Type} tuples, e.g. [{signups, counter}, {downloads, counter}]. Type
%% must be one of gauge, counter, derive or absolute.
create(RRD, Path, Service) ->
    log4erl:debug("Creating RRD for ~p service at ~p", [Service, Path]),
    send_command(RRD, make_rrd_create(Path, Service)).

%% @doc Update an RRD file with a new sample.
update(RRD, #sample{node=Node, service=Service, values=Values}) ->
    Path = rrd_path(Node, Service),
    send_command(RRD, make_update(Path, Values)).

%% ===================================================================
%% Utility functions
%% ===================================================================

%% @doc Send command to RRD server, return ok or {error, Reason}.
send_command(RRD, Cmd) ->
    FormattedCmd = errd_command:format(Cmd),
    case errd_server:raw(RRD, FormattedCmd) of
        {error, Reason} ->
            log4erl:error("RRD error: ~p", [Reason]),
            {error, Reason};
        {ok, _Lines} ->
            ok
    end.

%% @doc Sane string formatting.
string_format(Pattern, Values) ->
    lists:flatten(io_lib:format(Pattern, Values)).

%% @doc Create absolute path for RRD file for Service running on Node. A single
%% RRD file contains values for multiple metrics (data sources).
rrd_path(Node, Service) ->
    Filename = string:join([atom_to_list(Service#service.name), ?RRD_EXT], "."),
    filename:join([?RRD_DIR, atom_to_list(Node), Filename]).

%% @doc Generate command to create an RRD with a set of metrics.
make_rrd_create(Path, #service{frequency=Frequency, timeout=Timeout,
        archives=Archives, metrics=Metrics}) ->
    RRAs = [make_rra(S, C) || {S, C} <- Archives],
    DSs = [make_ds(M, Timeout) || M <- Metrics],
    #rrd_create{file=Path, step=Frequency, ds_defs=DSs, rra_defs=RRAs}.

%% @doc Create an average rrd_rra record from a step and a count.
make_rra(Step, Count) ->
    #rrd_rra{cf=average, args=string_format("0.5:~b:~b", [Step, Count])}.

%% @doc Make a rrd_ds record from a metric definition.
make_ds(#metric{name=Name, type=Type}, Timeout) ->
    #rrd_ds{name=atom_to_list(Name), type=Type, args=string_format("~b:U:U", [Timeout])}.

%% @doc Make an rrd_update record from an RRD path and a set of values.
make_update(Path, Values) ->
    Updates = [make_ds_update(M, V) || {M, V} <- Values],
    #rrd_update{file=Path, updates=Updates}.

%% @doc Make an rrd_ds_update record for a measurement.
make_ds_update(Metric, Value) ->
    #rrd_ds_update{name=atom_to_list(Metric), value=Value}.

%% ===================================================================
%% Tests
%% ===================================================================

string_format_test() ->
    ?assertEqual("X:1:9.5:z", string_format("~s:~b:~.1f:~p", ["X", 1, 9.5, z])).

rrd_path_test() ->
    Path = rrd_path(frank@josie, #service{name=loadtime}),
    ?assertEqual(filename:join([?RRD_DIR, "frank@josie", "loadtime.rrd"]), Path).

make_rrd_create_test() ->
    Path = filename:join([?RRD_DIR, "frank@josie", "loadtime.rrd"]),
    DSs = [#rrd_ds{name="loadtime", type=gauge, args="900:U:U"}],
    RRAs = [#rrd_rra{cf=average, args="0.5:1:60"}],
    Metrics = [#metric{name=loadtime, type=gauge}],
    Create = make_rrd_create(Path, #service{frequency=60, timeout=900,
            archives=[{1, 60}], metrics=Metrics}),
    ?assertEqual(#rrd_create{file=Path, step=60, ds_defs=DSs, rra_defs=RRAs}, Create).

make_update_test() ->
    Path = filename:join([?RRD_DIR, "frank@josie", "loadtime.rrd"]),
    Update = make_update(Path, [{loadtime, 0.99}]),
    DSUpdates = [#rrd_ds_update{name="loadtime", value=0.99}],
    Expected = #rrd_update{file=Path, updates=DSUpdates},
    ?assertEqual(Expected, Update).
