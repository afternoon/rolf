%% @doc gen_server which provides monitoring information for a single service on
%% a machine.
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

-module(rolf_service).
-behaviour(gen_server).
-export([
        % api
        start_link/1, stop/1, publish/1, get_state/1,
        % gen_server
        init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
        code_change/3,
        % utils
        invoke/2, start_emitting/1, stop_emitting/1]).

-include("rolf.hrl").

-define(PLUGIN_DIR, filename:join("priv", "plugin.d")).

%% ===================================================================
%% API
%% ===================================================================

%% @doc Start a service using Service as initial state.
start_link(Service) ->
    gen_server:start_link({local, server_name(Service)}, ?MODULE, [Service], []).

%% @doc Stop service Name.
stop(Name) -> gen_server:call(server_name(Name), stop).

%% @doc Trigger polling of this service manually, useful for inspecting and debugging
publish(Name) -> gen_server:cast(server_name(Name), publish).

%% @doc Get internal state - for debugging.
get_state(Name) -> gen_server:call(server_name(Name), get_state).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

%% @doc Start service, create a timer which will sample for results regularly and
%% publish them to the recorder.
init([Service]) ->
    error_logger:info_report({rolf_service, node(), init, Service}),
    start_emitting(Service),
    {ok, Service}.

handle_call(get_state, _From, Service) ->
    {reply, Service, Service};

handle_call(stop, _From, Service) ->
    error_logger:info_report({rolf_service, node(), stop}),
    {stop, normal, stopped, Service}.

handle_cast(publish, Service) ->
    [M, F, A] = Service#service.cmd,
    Samples = apply(M, F, [Service|A]),
    send(Samples),
    {noreply, Service}.

handle_info(Info, Ref) ->
    error_logger:info_report({rolf_service, node(), handle_info, Info}),
    {noreply, Ref}.

terminate(_Reason, Service) -> stop_emitting(Service).

code_change(_OldVsn, Service, _Extra) -> {ok, Service}.

%% ===================================================================
%% Utility functions
%% ===================================================================

%% @doc Get canonical name of service from name atom or service record.
server_name(Name) when is_atom(Name) ->
    list_to_atom(string:join(lists:map(fun atom_to_list/1, [?MODULE, Name]), "_"));
server_name(#service{name=Name}) ->
    server_name(Name).

%% @doc Send a set of samples to a client.
send(Samples) ->
    error_logger:info_report({rolf_service, node(), send, Samples}),
    rolf_recorder:store(Samples).

%% @doc Invoke plug-in and return samples.
invoke(Service, Plugin) ->
    error_logger:info_report({rolf_service, node(), invoke, Plugin}),
    Prog = filename:join(?PLUGIN_DIR, atom_to_list(Plugin)),
    parse_output(Service, os:cmd(Prog)).

%% @doc Coerce a string to a float or an integer.
list_to_num(S) ->
    try list_to_float(S) catch
        error:badarg ->
            try list_to_integer(S) catch
                error:badarg -> error
            end
    end.

%% @doc Parse the name of a metric, e.g. "loadtime.value" becomes the atom
%% loadtime.
parse_name(S) ->
    list_to_atom(hd(string:tokens(S, "."))).

parse_output(#service{name=Name}, Output) ->
    error_logger:info_report({parse_output, Name, Output}),
    % TODO actually parse the output
    Lines = string:tokens(Output, "\n"),
    Pairs = lists:map(fun(P) -> list_to_tuple(string:tokens(P, " ")) end, Lines),
    Values = lists:map(fun({K, V}) -> {parse_name(K), list_to_num(V)} end, Pairs),
    [#sample{nodename=node(), service=Name, values=Values}].

start_emitting(Service) ->
    error_logger:info_report({rolf_service, node(), start_emitting}),
    Name = Service#service.name,
    Freq = Service#service.freq,
    case timer:apply_interval(Freq, ?MODULE, publish, [Name]) of
        {ok, TRef} ->
            {ok, Service#service{tref=TRef}};
        _ ->
            error
    end.

stop_emitting(Service) ->
    error_logger:info_report({rolf_service, node(), stop_emitting}),
    timer:cancel(Service#service.tref).
