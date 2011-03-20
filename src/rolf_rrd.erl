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
-export([update/2]).

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
create_command(Path, Metrics) ->
    Step = 10,
    DSs = lists:map(fun({N, T}) -> #rrd_ds{name=N, type=T, args="900:U:U"} end,
                    Metrics),
    RRAs = [#rrd_rra{cf=average, args="0.5:1:360"},     % 1hr of 10s averages
            #rrd_rra{cf=average, args="0.5:30:288"},    % 1d of 5m averages
            #rrd_rra{cf=average, args="0.5:180:336"},   % 7d of 30m averages
            #rrd_rra{cf=average, args="0.5:8640:365"}], % 1y of 1d averages
    Cmd = #rrd_create{file=Path, step=Step, ds_defs=DSs, rra_defs=RRAs},
    error_logger:info_report({rolf_rrd, create_command, Cmd}),
    Cmd.

%% @doc Create an RRD file for a service. Metrics should be a list of
%% {Name, Type} tuples, e.g. [{signups, counter}, {downloads, counter}]. Type
%% must be one of gauge, counter, derive or absolute.
create(RRD, Path, Metrics) ->
    send_command(RRD, create_command(Path, Metrics)).

%% @doc Ensure data dir and RRD file for Service on Node exist.
ensure(RRD, Path, Metrics) ->
    error_logger:info_report({rolf_rrd, ensure, path, Path}),
    case filelib:ensure_dir(Path) of
        {error, Reason} ->
            error_logger:error_report({rolf_rrd, ensure, Reason}),
            {error, Reason};
        ok ->
            case filelib:is_file(Path) of
                false -> create(RRD, Path, Metrics);
                true ->  ok
            end
    end.

%% @doc Update an RRD file with new values.
update_file(RRD, Path, Values) ->
    Updates = lists:map(fun({M, V}) -> #rrd_ds_update{name=atom_to_list(M), value=V} end, Values),
    Cmd = #rrd_update{file=Path, updates=Updates},
    send_command(RRD, Cmd).

%% @doc Update an RRD file with a new sample.
update(RRD, #sample{nodename=Node, service=Service, values=Values}) ->
    Path = rrd_path(Node, Service),
    Metrics = lists:map(fun({M, _V}) -> {M, counter} end, Values),
    ensure(RRD, Path, Metrics),
    update_file(RRD, Path, Values).
