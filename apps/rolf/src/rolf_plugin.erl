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
-export([list/0, load/2]).

-include("rolf.hrl").

%% ===================================================================
%% Configuration
%% ===================================================================

%% @doc List available plugins.
list() ->
    {ok, PluginDir} = application:get_env(plugin_dir),
    list(PluginDir).

%% @doc List available plugins in Dir.
list(Dir) ->
    ConfigPat = filename:join([Dir, "*", "*.config"]),
    Configs = filelib:wildcard(ConfigPat),
    [configfilename_to_atom(C) || C <- Configs].

%% @doc Translate a config file pathname to an atom
configfilename_to_atom(CFName) ->
    list_to_atom(filename:rootname(filename:basename(CFName))).

%% @doc Load plugin config from file.
load(Plugin, Opts) ->
    {ok, Config} = file:consult(config_path(Plugin)),
    parse(Plugin, propmerge(Config, Opts)).

%% @doc Get path to a plugin's config file.
config_path(Plugin) ->
    PluginStr = atom_to_list(Plugin),
    CfgName = string:join([PluginStr, "config"], "."),
    {ok, PluginDir} = application:get_env(plugin_dir),
    filename:join([PluginDir, PluginStr, CfgName]).

%% @doc Parse config file contents into a service record.
parse(Plugin, Config) ->
    {ok, PluginDefaultFreq} = application:get_env(plugin_default_freq),
    {ok, PluginDefaultTimeoutMultiple} = application:get_env(plugin_default_timeout_multiple),
    {ok, PluginDefaultArchives} = application:get_env(plugin_default_archives),
    Freq = proplists:get_value(frequency, Config, PluginDefaultFreq),
    #service{
        plugin=Plugin,
        module=proplists:get_value(module, Config, rolf_command),
        command=parse_command(Plugin, Config),
        frequency=Freq,
        timeout=proplists:get_value(timeout, Config, Freq * PluginDefaultTimeoutMultiple),
        archives=proplists:get_value(archives, Config, PluginDefaultArchives),
        graph_title=proplists:get_value(graph_title, Config, atom_to_list(Plugin)),
        graph_vlabel=proplists:get_value(graph_vlabel, Config, ""),
        metrics=parse_metrics(Config),
        config=Config
    }.

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
    {ok, PluginDir} = application:get_env(plugin_dir),
    filename:join([PluginDir, atom_to_list(Plugin), Cmd]).

%% @doc Parse config for a list of metrics into list of metric records.
parse_metrics(Config) ->
    MetricCfg = proplists:get_value(metrics, Config, []),
    [parse_metric(M) || M <- MetricCfg].

%% @doc Parse config for a single metric into a metric record.
parse_metric({Metric, MetricCfg}) ->
    {ok, PluginDefaultType} = application:get_env(plugin_default_type),
    {ok, PluginDefaultDraw} = application:get_env(plugin_default_draw),
    #metric{
        name=Metric,
        label=proplists:get_value(label, MetricCfg, ""),
        type=proplists:get_value(type, MetricCfg, PluginDefaultType),
        draw=proplists:get_value(draw, MetricCfg, PluginDefaultDraw),
        min=proplists:get_value(min, MetricCfg, undefined),
        max=proplists:get_value(max, MetricCfg, undefined),
        colour=proplists:get_value(colour, MetricCfg, undefined)
    }.

propmerge(L1, L2) ->
    dict:to_list(dict:merge(fun(_K, _V1, V2) -> V2 end, dict:from_list(L1), dict:from_list(L2))).

%% ===================================================================
%% Tests
%% ===================================================================

configfilename_to_atom_test() ->
    ?assertEqual(disk, configfilename_to_atom("plugins/disk/disk.config")).

config_path_test() ->
    {ok, PluginDir} = application:get_env(plugin_dir),
    Path = filename:join([PluginDir, "loadtime", "loadtime.config"]),
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
    ?assertEqual(loadtime, Output#service.plugin),
    ?assertEqual(undefined, Output#service.name),
    ?assertEqual(10, Output#service.frequency),
    ?assertEqual("plugins/loadtime/loadtime.sh", Output#service.command),
    ?assertEqual(rolf_command, Output#service.module).

parse_options_test() ->
    Input = [{command, "loadtime.sh"},
             {unit, mb}],
    Output = parse(loadtime, Input),
    ?assertEqual(mb, proplists:get_value(unit, Output#service.config)).

propmerge_test() ->
    ?assertEqual([{a, 1}, {b, 2}], propmerge([{a, 1}], [{b, 2}])),
    ?assertEqual([{a, 2}], propmerge([{a, 1}], [{a, 2}])).
