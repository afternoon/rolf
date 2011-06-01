%% @doc Plugin module for measuring web site load time.
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

-module(rolf_loadtime).
-behaviour(rolf_collector).

%% rolf_collector callbacks
-export([start/1, collect/2, stop/2]).

%% helpers
-export([time_url/1]).

-include("rolf.hrl").

%% ===================================================================
%% rolf_collector callbacks
%% ===================================================================

%% @doc Start collector.
start(_Service) ->
    inets:start().

%% @doc HTTP load time collector function for Rolf. Options should contain a key
%% urls with value [{Name, Url}].
collect(Service, State) ->
    Config = Service#service.config,
    Url = proplists:get_value(url, Config, []),
    Values = [{loadtime, time_url(Url)}],
    Sample = #sample{node=node(), service=Service, values=Values},
    {State, Sample}.

%% @doc Stop collector.
stop(_Service, _State) ->
    inets:stop().

%% ===================================================================
%% Helper functions
%% ===================================================================

%% @doc Return how long it took to load resource at Url in milliseconds.
time_url(Url) ->
    {T, _} = timer:tc(httpc, request, [head, {Url, []}, [], []]),
    round(T / 1000).
