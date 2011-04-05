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

%% API
-export([start_link/1, stop/1, publish/1, invoke/3, server_name/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-include("rolf.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc Start a service using Service as initial state.
start_link(Service) ->
    error_logger:info_report([{where, {node(), rolf_service, start_link}}, {service, Service}]),
    gen_server:start_link({local, server_name(Service)}, ?MODULE, [Service], []).

%% @doc Stop service Name.
stop(Name) ->
    gen_server:call({local, server_name(Name)}, stop).

%% @doc Trigger polling of this service manually, useful for inspecting and debugging
publish(Name) ->
    gen_server:cast({local, server_name(Name)}, publish).

%% @doc Start emitting samples. Emit one straight away and then set a timer to
%% emit regularly.
start_emitting(Name) ->
    gen_server:cast({local, server_name(Name)}, start_emitting).

%% @doc Stop emitting samples.
stop_emitting(Name) ->
    gen_server:cast({local, server_name(Name)}, stop_emitting).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

%% @doc Start service, create a timer which will sample for results regularly and
%% publish them to the recorder.
init([Service]) ->
    process_flag(trap_exit, true),
    {start_emitting(Service), Service}.

handle_call(_Req, _From, Service) ->
    {reply, Service}.

handle_cast(publish, Service) ->
    {M, F, A} = Service#service.mfa,
    FullArgs = [Service|A],
    Sample = apply(M, F, FullArgs),
    rolf_recorder:store(Sample),
    {noreply, Service};

handle_cast(start_emitting, Service) ->
    Name = Service#service.name,
    Freq = Service#service.frequency * 1000,
    error_logger:info_report([{where, {node(), rolf_service, start_emitting}}, {name, Name}, {freq, Freq}]),
    apply(?MODULE, publish, [Name]),
    case timer:apply_interval(Freq, ?MODULE, publish, [Name]) of
        {ok, TRef} ->
            {noreply, Service#service{tref=TRef}};
        _ ->
            {noreply, Service}
    end;

handle_cast(stop_emitting, Service) ->
    Name = Service#service.name,
    error_logger:info_report([{where, {rolf_service, node(), start_emitting}}, {name, Name}]),
    timer:cancel(Service#service.tref),
    {noreply, Service#service{tref=undef}}.

handle_info(_Info, Ref) ->
    {noreply, Ref}.

terminate(_Reason, Service) ->
    stop_emitting(Service).

code_change(_OldVsn, Service, _Extra) -> {ok, Service}.

%% ===================================================================
%% Utility functions
%% ===================================================================

%% @doc Get canonical name of service from name atom or service record.
server_name(Name) when is_atom(Name) ->
    Module = atom_to_list(?MODULE),
    StrName = atom_to_list(Name),
    list_to_atom(string:join([Module, StrName], "_"));
server_name(#service{name=Name}) ->
    server_name(Name).

%% ===================================================================
%% Command functions
%% ===================================================================

%% @doc Invoke plug-in and return samples.
invoke(Service, Cmd, Args) ->
    FullCmd = string:join([Cmd, Args], " "),
    parse_output(Service#service.name, os:cmd(FullCmd)).

%% @doc Parse output from external command.
parse_output(Name, Output) ->
    Values = [parse_line(Line) || Line <- split_lines(Output)],
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

%% ===================================================================
%% Tests
%% ===================================================================

server_name_test() ->
    Name = list_to_atom("rolf_service_loadtime"),
    ?assertEqual(Name, server_name(loadtime)),
    ?assertEqual(Name, server_name(#service{name=loadtime})).

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
