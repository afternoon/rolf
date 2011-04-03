%% @doc Rolf supervisor configuration.
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

-module(rolf_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

-include("rolf.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({global, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%% @doc Master supervisor for rolf application. Start a recorder and a second
%% supervisor which oversees creation of the cluster and starting of a service
%% supervisor per-node.
init([]) ->
    Recorder = {rolf_recorder, {rolf_recorder, start_link, []},
                               permanent, 5000, worker, [rolf_recorder]},
    NodeSupervisor = {rolf_node_sup, {rolf_node_sup, start_link, []},
                                     permanent, 5000, supervisor, [rolf_node_sup]},
    {ok, {{one_for_one, 3, 5}, [Recorder, NodeSupervisor]}}.
