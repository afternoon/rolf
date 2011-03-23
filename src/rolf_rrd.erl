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
-export([ensure/3, update/2]).

-include_lib("errd/include/errd.hrl").
-include("rolf.hrl").

-define(RRD_DIR, filename:join("priv", "data")).
-define(RRD_EXT, "rrd").

%% @doc Create absolute path for RRD file for Service running on Node. A single
%% RRD file contains values for multiple metrics (data sources).
rrd_path(Node, Service) ->
    Filename = string:join([atom_to_list(Service), ?RRD_EXT], "."),
    filename:join([?RRD_DIR, atom_to_list(Node), Filename]).

%% @doc Send command to RRD server, return ok or {error, Reason}.
send_command(RRD, Cmd) ->
    case errd_server:command(RRD, Cmd) of
        {error, Reason} ->
            error_logger:error_report({rolf_rrd, send_command, Reason}),
            {error, Reason};
        {ok, Lines} ->
            error_logger:info_report({rolf_rrd, send_command, Lines}),
            ok
    end.

%% @doc Generate command to create an RRD with a set of metrics.
create_command(Path, #service{frequency=Frequency, timeout=Timeout,
        archives=Archives, metrics=Metrics}) ->
    % TODO move RRA to plugin config, rewrite this to use config format
    RRAs = lists:map(fun({S, C}) -> make_rra(S, C) end, Archives),
    DSs = lists:map(fun(M) -> make_ds(M, Timeout) end, Metrics),
    Cmd = #rrd_create{file=Path, step=Frequency, ds_defs=DSs, rra_defs=RRAs},
    error_logger:info_report({rolf_rrd, create_command, Cmd}),
    Cmd.

%% @doc Create an RRD file for a service. Metrics should be a list of
%% {Name, Type} tuples, e.g. [{signups, counter}, {downloads, counter}]. Type
%% must be one of gauge, counter, derive or absolute.
create(RRD, Path, Service) ->
    send_command(RRD, create_command(Path, Service)).

%% @doc Ensure data dir and RRD file for Service on Node exist.
ensure(RRD, Node, Service) ->
    Path = rrd_path(Node, Service),
    error_logger:info_report({rolf_rrd, ensure, path, Path}),
    case filelib:ensure_dir(Path) of
        {error, Reason} ->
            error_logger:error_report({rolf_rrd, ensure, Reason}),
            {error, Reason};
        ok ->
            case filelib:is_file(Path) of
                false -> create(RRD, Path, Service);
                true ->  ok
            end
    end.

%% @doc Update an RRD file with a new sample.
update(RRD, #sample{node=Node, service=Service, values=Values}) ->
    Path = rrd_path(Node, Service),
    Updates = lists:map(fun({M, V}) -> #rrd_ds_update{name=atom_to_list(M), value=V} end, Values),
    Cmd = #rrd_update{file=Path, updates=Updates},
    send_command(RRD, Cmd).

%% ===================================================================
%% Utility functions
%% ===================================================================

string_format(Pattern, Values) ->
    lists:flatten(io_lib:format(Pattern, Values)).

%% @doc Create an average rrd_rra record from a step and a count.
make_rra(Step, Count) ->
    #rrd_rra{cf=average, args=string_format("0.5:~p:~p", [Step, Count])}.

%% @doc Make a rrd_ds record from a metric definition.
make_ds(#metric{name=Name, type=Type}, Timeout) ->
    #rrd_ds{name=Name, type=Type, args=string_format("~p:U:U", Timeout)}.
