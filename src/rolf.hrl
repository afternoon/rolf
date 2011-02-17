%% @doc Rolf records.
%% @author Ben Godfrey <ben@ben2.com> [http://aftnn.org/]
%% @copyright 2011 Ben Godfrey
%% @version 1.0.0

-record(server, {clients, services}).
-record(service, {name, cmd, freq, clients, tref}).
