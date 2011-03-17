%% @doc Rolf records.
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

%% @doc State record for Rolf recorder.
-record(recorder, {errdserver=undef}).

%% @doc State record for Rolf nodes.
-record(node, {recorders=[], services=[]}).

%% @doc State record for Rolf services.
-record(service, {name=undef, cmd=undef, freq=undef, recorders=[], tref=undef}).

%% @doc Record for Rolf samples.
-record(sample, {nodename=undef, service=undef, datetime=undef, value=undef}).
