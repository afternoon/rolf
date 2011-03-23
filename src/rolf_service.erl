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
        invoke/3, start_emitting/1, stop_emitting/1]).

-include("rolf.hrl").

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
    {M, F, A} = Service#service.mfa,
    Sample = apply(M, F, [Service|A]),
    error_logger:info_report({rolf_service, node(), sending, Sample}),
    rolf_recorder:store(Sample),
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

%% @doc Invoke plug-in and return samples.
%% @todo Portify to handle daemonization.
invoke(Service, Cmd, Args) ->
    error_logger:info_report({rolf_service, node(), invoke, Cmd, Args}),
    Cmd = string:join([Cmd, Args], " "),
    parse_output(Service#service.name, os:cmd(Cmd)).

%% @doc Parse output from external command.
parse_output(Name, Output) ->
    Lines = split_lines(Output),
    Values = lists:map(fun parse_line/1, Lines),
    #sample{node=node(), service=Name, values=Values}.

%% @doc Split output into lines, drop terminating ".\n" line.
split_lines(Lines) ->
    Lines1 = string:tokens(Lines, "\n"),
    lists:filter(fun(L) -> L /= "." end, Lines1).

%% @doc Parse line into {atom, int_or_float} tuple/
parse_line(Line) ->
    {K, V} = list_to_tuple(string:tokens(Line, " ")),
    {list_to_atom(K), list_to_num(V)}.

%% @doc Coerce a string to a float or an integer.
list_to_num(S) ->
    try list_to_float(S) catch
        error:badarg ->
            try list_to_integer(S) catch
                error:badarg -> error
            end
    end.

%% @doc Start emitting samples.
start_emitting(Service) ->
    error_logger:info_report({rolf_service, node(), start_emitting}),
    Name = Service#service.name,
    Freq = Service#service.frequency,
    case timer:apply_interval(Freq, ?MODULE, publish, [Name]) of
        {ok, TRef} ->
            {ok, Service#service{tref=TRef}};
        _ ->
            error
    end.

%% @doc Stop emitting samples.
stop_emitting(Service) ->
    error_logger:info_report({rolf_service, node(), stop_emitting}),
    timer:cancel(Service#service.tref).

%% ===================================================================
%% Tests
%% ===================================================================

server_name_test() ->
    ?assertEqual(rolf_service_loadtime, server_name(loadtime)),
    ?assertEqual(rolf_service_loadtime, server_name(#service{name=loadtime})).

list_to_num_test() ->
    ?assertEqual(99, list_to_num("99")),
    ?assertEqual(-1, list_to_num("-1")),
    ?assertEqual(0.999, list_to_num("0.999")),
    ?assertEqual(-3.14, list_to_num("-3.14")),
    ?assertEqual(error, list_to_num("monkey")).

split_lines_test() ->
    Result = split_lines("loadtime 0.99\n.\n"),
    ?assertEqual(["loadtime 0.99"], Result).

parse_line_test() ->
    Result = parse_line("loadtime 0.99"),
    ?assertEqual({loadtime, 0.99}, Result).

parse_output_test() ->
    Result = parse_output(loadtime, "loadtime 0.99\n.\n"),
    ?assertEqual(#sample{node=node(), service=loadtime, values=[{loadtime, 0.99}]}, Result).

parse_output_many_test() ->
    Result = parse_output(loadtime, "loadtime 0.99\nttfb 0.65\nrendertime 2\n.\n"),
    ?assertEqual(#sample{node=node(), service=loadtime, values=[{loadtime,
                        0.99}, {ttfb, 0.65}, {rendertime, 2}]}, Result).
