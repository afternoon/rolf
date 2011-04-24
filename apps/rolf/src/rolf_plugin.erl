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

%% API
-export([list/0, load/1]).

-include("rolf.hrl").

-define(PLUGIN_DIR, filename:join("priv", "plugin.d")).
-define(PLUGIN_DEFAULT_FREQ, 10).
-define(PLUGIN_DEFAULT_TIMEOUT_MULTIPLE, 3).
-define(PLUGIN_DEFAULT_ARCHIVES, [{1, 360},      % 1hr of 10s averages
                                  {30, 288},     % 1d of 5m averages
                                  {180, 336},    % 7d of 30m averages
                                  {8640, 365}]). % 1y of 1d averages
-define(PLUGIN_DEFAULT_TYPE, gauge).
-define(PLUGIN_DEFAULT_DRAW, line).

%% ===================================================================
%% Configuration
%% ===================================================================

%% @doc List available plugins.
list() -> list(?PLUGIN_DIR).

%% @doc List available plugins in Dir.
list(Dir) ->
    ConfigPat = filename:join([Dir, "*", "*.config"]),
    Configs = filelib:wildcard(ConfigPat),
    [configfilename_to_atom(C) || C <- Configs].

%% @doc Translate a config file pathname to an atom
configfilename_to_atom(CFName) ->
    list_to_atom(filename:rootname(filename:basename(CFName))).

%% @doc Load plugin config from file.
load(Plugin) ->
    {ok, Config} = file:consult(config_path(Plugin)),
    parse(Plugin, Config).

%% @doc Get path to a plugin's config file.
config_path(Plugin) ->
    PluginStr = atom_to_list(Plugin),
    CfgName = string:join([PluginStr, "config"], "."),
    filename:join([?PLUGIN_DIR, PluginStr, CfgName]).

%% @doc Parse the command for this plugin.
parse_command(Plugin, Config) ->
    case proplists:get_value(command, Config, undefined) of
        undefined ->
            undefined;
        Cmd ->
            external_path(Plugin, Cmd)
    end.

%% @doc Return the full path to an external program.
external_path(Plugin, Cmd) ->
    filename:join([?PLUGIN_DIR, atom_to_list(Plugin), Cmd]).

%% @doc Parse config file contents into a service record.
parse(Plugin, Config) ->
    Freq = proplists:get_value(frequency, Config, ?PLUGIN_DEFAULT_FREQ),
    #service{
        name=Plugin,
        module=proplists:get_value(module, Config, rolf_command),
        command=parse_command(Plugin, Config),
        frequency=Freq,
        timeout=proplists:get_value(timeout, Config, Freq * ?PLUGIN_DEFAULT_TIMEOUT_MULTIPLE),
        archives=proplists:get_value(archives, Config, ?PLUGIN_DEFAULT_ARCHIVES),
        graph_title=proplists:get_value(graph_title, Config, atom_to_list(Plugin)),
        graph_vlabel=proplists:get_value(graph_vlabel, Config, ""),
        metrics=parse_metrics(Config),
        options=proplists:get_value(options, Config, [])
    }.

%% @doc Parse config for a list of metrics into list of metric records.
parse_metrics(Config) ->
    MetricCfg = proplists:get_value(metrics, Config, []),
    [parse_metric(M) || M <- MetricCfg].

%% @doc Parse config for a single metric into a metric record.
parse_metric({Metric, MetricCfg}) ->
    #metric{
        name=Metric,
        label=proplists:get_value(label, MetricCfg, ""),
        type=proplists:get_value(type, MetricCfg, ?PLUGIN_DEFAULT_TYPE),
        draw=proplists:get_value(draw, MetricCfg, ?PLUGIN_DEFAULT_DRAW),
        min=proplists:get_value(min, MetricCfg, undefined),
        max=proplists:get_value(max, MetricCfg, undefined),
        colour=proplists:get_value(colour, MetricCfg, undefined)
    }.

%% ===================================================================
%% Tests
%% ===================================================================

configfilename_to_atom_test() ->
    ?assertEqual(disk, configfilename_to_atom("priv/plugin.d/disk/disk.config")).

list_test() ->
    ?assertEqual([disk, loadtime], list("../priv/plugin.d")).

config_path_test() ->
    Path = filename:join([?PLUGIN_DIR, "loadtime", "loadtime.config"]),
    ?assertEqual(Path, config_path(loadtime)).

parse_test() ->
    Input = [{command, "loadtime.sh"},
             {frequency, 10},
             {graph_title, "Load Time"},
             {graph_vlabel, "Secs"},
             {metrics, [{loadtime, [{label, "Load Time"},
                                    {type, gauge},
                                    {draw, areastack},
                                    {min, 0},
                                    {colour, "#0091FF"}]}]}],
    Output = parse(loadtime, Input),
    ?assertEqual(loadtime, Output#service.name),
    ?assertEqual(10, Output#service.frequency),
    ?assertEqual("priv/plugin.d/loadtime/loadtime.sh", Output#service.command),
    ?assertEqual(rolf_command, Output#service.module).

parse_options_test() ->
    Input = [{command, "loadtime.sh"},
             {options, [{unit, mb}]}],
    Output = parse(loadtime, Input),
    ?assertEqual([{unit, mb}], Output#service.options).
