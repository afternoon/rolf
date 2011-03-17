%% @doc Utilities for managing RRD files with errd.
%% @author Ben Godfrey <ben@ben2.com> [http://aftnn.org/]
%% @copyright 2011 Ben Godfrey
%% @version 1.0.0
%%
%% Rolf - a system monitoring and graphing tool like Munin or collectd.
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

-module(rolf_rrd_utils).
-export([update_rrd/2]).
-include("rolf.hrl").

-define(RRD_DIR, filename:join("priv", "data")).
-define(RRD_EXT, "rrd").

%% @doc Ensure data dir and RRD file for Service on Node exist.
ensure_rrd(ErrdServer, NodeName, ServiceName, MetricName, Type) ->
    Dir = filename:join([?RRD_DIR, NodeName, ServiceName]),
    error_logger:info_report({rolf_rrd_utils, ensure_rrd, dir, Dir}),
    case filelib:ensure_dir(Dir ++ "/") of
        {error, Reason} -> {error, Reason};
        ok ->
            Filename = rrd_filename(Dir, MetricName),
            case filelib:is_file(Filename) of
                false -> create_rrd(ErrdServer, Filename, MetricName, Type);
                true -> ok
            end
    end.

%% @doc Create name for RRD file for Service running on Node.
rrd_filename(Dir, MetricName) ->
    filename:join(Dir, string:join([atom_to_list(MetricName), ?RRD_EXT], ".")).

%% @doc Create an RRD file using errd_server.
create_rrd(ErrdServer, Filename, Name, Type)
        when Type == gauge; Type == counter; Type == derive; Type == absolute ->
    Cmd = errd_command:create(Filename, Name, Type),
    error_logger:info_report({rolf_rrd_utils, create_rrd, cmd, Cmd}),
    errd_server:command(ErrdServer, Cmd),
    ok.

%% @doc Update an RRD file with a new sample.
update_rrd(ErrdServer, #sample{node=Node, service=Service, datetime=_DateTime,
        value=_Value}) ->
    NodeName = atom_to_list(Node),
    ensure_rrd(ErrdServer, NodeName, Service, foos, gauge),
    ok.
