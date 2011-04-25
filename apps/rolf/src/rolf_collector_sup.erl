%% @doc Rolf collector node supervisor.
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

-module(rolf_collector_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_services/1]).

%% supervisor callbacks
-export([init/1]).

-include("rolf.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%% @doc Collector supervisor initially does nothing. The recorder will send
%% configuration.
init([]) ->
    SupFlags = {simple_one_for_one, 1, 10},
    ChildTemplate = {rolf_service,
                     {rolf_service, start_link, []},
                     permanent, 2000, worker, [rolf_service]},
    {ok, {SupFlags, [ChildTemplate]}}.

%% @doc Start a set of service process. Called by
%% rolf_recorder:start_collectors.
start_services([]) -> ok;
start_services([S|Services]) ->
    error_logger:info_report([{where, {node(), rolf_collector_sup, start_services}}, {service, S}]),
    supervisor:start_child(?MODULE, [S]),
    rolf_service:start_emitting(S#service.name),
    start_services(Services).
