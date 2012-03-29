-module(to).
-export([atom/1, binary/1, integer/1, string/1]).
-export([seconds/1, milliseconds/1]).
-export([lower/1, upper/1]).
-ignore_xref([atom/1, binary/1, integer/1, string/1,
	      seconds/1, milliseconds/1,
	      lower/1, upper/1
	     ]).

-type atomable()    :: stringable().
-type binable()     :: stringable().
-type integerable() :: stringable(). %% if convertable to an integer
-type stringable()  :: integer() | atom() | binary() | io_lib:chars().

-spec atom(atomable()) -> atom().
atom(V) when is_atom(V) -> V;
atom(V) -> list_to_atom(string(V)).

-spec binary(binable()) -> binary().
binary(V) -> list_to_binary(string(V)).

-spec integer(integerable()) -> integer().
integer(V) when is_integer(V) -> V;
integer(V) -> list_to_integer(string(V)).

-spec string(stringable()) -> string().
string(V) when is_atom(V) -> atom_to_list(V);
string(V) when is_integer(V) -> integer_to_list(V);
string(V) when is_binary(V) -> string(binary_to_list(V)); %% Assuming "nice" binary
string(V) when is_list(V) ->
    true = io_lib:deep_char_list(V),
    lists:flatten(V).

-spec lower(atom()) -> atom(); (stringable()) -> string().
lower(V) when is_atom(V) ->
    atom(lower(string(V)));
lower(V) ->
    string:to_lower(string(V)).

-spec upper(atom()) -> atom(); (stringable()) -> string().
upper(V) when is_atom(V) ->
    atom(upper(string(V)));
upper(V) ->
    string:to_upper(string(V)).

-type time_unit() :: day | days | hour | hours | minute | minutes
		   | second | seconds | millisecond | milliseconds.
-type duration() :: integerable() | {integerable(), time_unit()}.

-spec milliseconds(duration()) -> integer().
milliseconds({N, day}) -> milliseconds({N, days});
milliseconds({N, days}) -> milliseconds({N * 24, hours});
milliseconds({N, hour}) -> milliseconds({N, hours});
milliseconds({N, hours}) -> milliseconds({N * 60, minutes});
milliseconds({N, minute}) -> milliseconds({N, minutes});
milliseconds({N, minutes}) -> milliseconds({N * 60, seconds});
milliseconds({N, second}) -> milliseconds({N, seconds});
milliseconds({N, seconds}) -> milliseconds({N * 1000, milliseconds});
milliseconds({N, millisecond}) -> to:integer(N);
milliseconds({N, milliseconds}) -> to:integer(N);
milliseconds(V) -> to:integer(V).

-spec seconds(duration()) -> integer().
seconds({N, day}) -> seconds({N, days});
seconds({N, days}) -> seconds({N * 24, hours});
seconds({N, hour}) -> seconds({N, hours});
seconds({N, hours}) -> seconds({N * 60, minutes});
seconds({N, minute}) -> seconds({N, minutes});
seconds({N, minutes}) -> seconds({N * 60, seconds});
seconds({N, second}) -> to:integer(N);
seconds({N, seconds}) -> to:integer(N);
seconds({N, millisecond}) -> seconds({N, milliseconds});
seconds({N, milliseconds}) -> seconds({N / 1000, seconds});
seconds(V) -> to:integer(V).
