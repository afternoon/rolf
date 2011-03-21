%% @doc Represent a service plugin, which gathers data.
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

-module(rolf_plugin).
-export([load/1]).

-include_lib("eunit/include/eunit.hrl").
-include("rolf.hrl").

-define(PLUGIN_DIR, filename:join("priv", "plugin.d")).
-define(PLUGIN_DEFAULT_FREQ, 10).
-define(PLUGIN_DEFAULT_TYPE, gauge).
-define(PLUGIN_DEFAULT_DRAW, line).

%% ===================================================================
%% Configuration
%% ===================================================================

%% @doc Load plugin config from file.
load(Plugin) ->
    CfgName = string:join([atom_to_list(Plugin), "config"], "."),
    Path = filename:join(?PLUGIN_DIR, CfgName),
    parse(Plugin, file:consult(Path)).

%% @doc Return the Value for Key in TupleList or a default value
keyfind_default(Key, TupleList, Default) ->
    case lists:keyfind(Key, 1, TupleList) of
        {Key, Value} -> Value;
        false -> Default
    end.

%% @doc Return the full path to an external program.
external_path(Plugin, Cmd) ->
    filename:join([?PLUGIN_DIR, atom_to_list(Plugin), Cmd]).

%% @doc Extract the MFA for this plugin
parse_mfa(Plugin, Cfg) ->
    case lists:keyfind(mfa, 1, Cfg) of
        {mfa, M, F, A} ->
            {M, F, A};
        false ->
            case lists:keyfind(external, 1, Cfg) of
                {external, Cmd, Args} ->
                    [rolf_service, invoke, [external_path(Plugin, Cmd), Args]];
                {external, Cmd} ->
                    [rolf_service, invoke, [external_path(Plugin, Cmd), []]];
                false ->
                    undefined
            end
    end.

%% @doc Parse config for a single metric into a metric record.
parse_metric({Metric, MetricCfg}) ->
    #metric{
        name=Metric,
        label=keyfind_default(label, MetricCfg, ""),
        type=keyfind_default(type, MetricCfg, ?PLUGIN_DEFAULT_TYPE),
        draw=keyfind_default(draw, MetricCfg, ?PLUGIN_DEFAULT_DRAW),
        min=keyfind_default(min, MetricCfg, undefined),
        max=keyfind_default(max, MetricCfg, undefined),
        colour=keyfind_default(colour, MetricCfg, undefined)
    }.

%% @doc Parse config for a list of metrics into list of metric records.
parse_metrics(Cfg) ->
    MetricCfg = keyfind_default(metrics, Cfg, []),
    lists:map(fun parse_metric/1, MetricCfg).

%% @doc Parse config file contents into a service record.
parse(Plugin, Cfg) ->
    #service{
        name=Plugin,
        mfa=parse_mfa(Plugin, Cfg),
        frequency=keyfind_default(frequency, Cfg, ?PLUGIN_DEFAULT_FREQ),
        title=keyfind_default(title, Cfg, atom_to_list(Plugin)),
        vlabel=keyfind_default(vlabel, Cfg, ""),
        metrics=parse_metrics(Cfg)
    }.

%% ===================================================================
%% Tests
%% ===================================================================

parse_mfa_test() ->
    Output = parse_mfa(loadtime, [{external, "loadtime.sh"}]),
    Args = [filename:join([?PLUGIN_DIR, "loadtime", "loadtime.sh"]), []],
    ?assertEqual([rolf_service, invoke, Args], Output).

parse_test() ->
    Input = [{external, "loadtime.sh"},
             {frequency, 10},
             {title, "Load Time"},
             {vlabel, "Secs"},
             {metrics, [{loadtime, [{label, "Load Time"},
                                    {type, gauge},
                                    {draw, areastack},
                                    {min, 0},
                                    {colour, "#0091FF"}]}]}],
    Output = parse(loadtime, Input),
    ?assertEqual(loadtime, Output#service.name).
