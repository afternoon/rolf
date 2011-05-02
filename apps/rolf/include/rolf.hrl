%% @doc Rolf records.
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

%% import eunit macros into all modules
-include_lib("eunit/include/eunit.hrl").

%% @doc State record for Rolf recorder.
-record(recorder, {collectors=[],
                   rrd=undefined}).

%% @doc State record for Rolf nodes.
-record(node, {services=[]}).

%% @doc State record for Rolf services, which contains multiple metrics.
-record(service, {name=undefined,
                  plugin=undefined,
                  module=undefined,
                  command=undefined,
                  frequency=undefined,
                  timeout=undefined,
                  archives=undefined,
                  graph_title=undefined,
                  graph_vlabel=undefined,
                  metrics=undefined,
                  config=undefined,
                  tref=undefined}).

%% @doc Record for a single metric.
-record(metric, {name=undefined,
                 label="",
                 type=gauge,
                 draw=line,
                 min=undefined,
                 max=undefined,
                 colour=undefined}).

%% @doc Record for Rolf samples. values is a list of tuples of format
%% {Metric, Value}.
-record(sample, {node=undefined,
                 service=undefined,
                 values=undefined}).
