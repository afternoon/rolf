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
-export([start/1, collect/1, stop/1]).

%% helpers
-export([time_urls/1, time_many/1, time_one/1]).

-include("rolf.hrl").

%% ===================================================================
%% rolf_collector callbacks
%% ===================================================================

%% @doc Start collector.
start(_Service) ->
    inets:start().

%% @doc HTTP load time collector function for Rolf. Options should contain a key
%% urls with value [{Name, Url}].
collect(Service) ->
    Config = Service#service.config,
    UrlConfig = proplists:get_value(urls, Config, []),
    Values = time_urls(UrlConfig),
    #sample{node=node(), service=Service, values=Values}.

%% @doc Stop collector.
stop(_Service) ->
    inets:stop().

%% ===================================================================
%% Helper functions
%% ===================================================================

%% @doc Time a set of urls. UrlConfig is [{Name, Url}] format.
time_urls(UrlConfig) ->
    {Names, Urls} = lists:unzip(UrlConfig),
    lists:zip(Names, time_many(Urls)).

%% @doc Time loading multiple urls. Run tests in parallel using plists.
time_many(Urls) ->
    plists:map(fun time_one/1, Urls).

%% @doc Return how long it took to load resource at Url in milliseconds.
time_one(Url) ->
    {T, _} = timer:tc(httpc, request, [head, {Url, []}, [], []]),
    round(T / 1000).
