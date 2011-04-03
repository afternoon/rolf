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

-module(rolf_service_sup).
-behaviour(supervisor).

%% API
-export([start_link/1]).

%% supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Services) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Services]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%% @doc Supervisor for instances of rolf_service.
init([Services]) ->
    ChildSpecs = [child_spec(S) || S <- Services],
    {ok, {{one_for_one, 3, 5}, ChildSpecs}}.

%% ===================================================================
%% Utility functions
%% ===================================================================

%% @doc Create a child spec for this node.
child_spec(Service) ->
    {rolf_service:server_name(Service),
     {rolf_service, start_link, [rolf_plugin:load(Service)]},
     permanent, 5000, worker, [rolf_service]}.
