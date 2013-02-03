%% Look at lists:concat!

%% Validate plugin values

%% @doc Validate a list of values returned by a plugin.
values_ok(Values, Metrics) when is_list(Values) ->
    lists:foldl(fun(V) -> value_ok(V, Metrics), Values);
values_ok(_, _Metrics) -> false.

%% @doc Validate a single value returned by a plugin.
value_ok({Name, Value}, Metrics) when is_atom(Name) and is_int(Value) ->
    lists:member(Name, Metrics);
value_ok(_, _Metrics) -> false.

%% Validate (normalised) configuration

%% - Node is atom()
%% - Plugins all exist
%% - Service names are unique
%% - Options is [{atom(), term()}]
