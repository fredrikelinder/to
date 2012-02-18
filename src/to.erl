-module(to).
-export([atom/1, integer/1, string/1]).
-export([seconds/1, milliseconds/1]).
-ignore_xref([atom/1, integer/1, string/1, seconds/1, milliseconds/1]).

atom(V) when is_atom(V) -> V;
atom(V) when is_list(V) -> list_to_atom(string(V));
atom(V) when is_integer(V) -> list_to_atom(string(V)).

integer(V) when is_atom(V) -> list_to_integer(string(V));
integer(V) when is_list(V) -> list_to_integer(string(V));
integer(V) when is_integer(V) -> V.

string(V) when is_atom(V) -> atom_to_list(V);
string(V) when is_integer(V) -> integer_to_list(V);
string(V) when is_list(V) ->
    true = io_lib:deep_char_list(V),
    lists:flatten(V).

milliseconds({N, day}) -> milliseconds({N * 24, hours});
milliseconds({N, days}) -> milliseconds({N * 24, hours});
milliseconds({N, hour}) -> milliseconds({N * 60, minutes});
milliseconds({N, hours}) -> milliseconds({N * 60, minutes});
milliseconds({N, minute}) -> milliseconds({N * 60, seconds});
milliseconds({N, minutes}) -> milliseconds({N * 60, seconds});
milliseconds({N, second}) -> milliseconds({N * 1000, milliseconds});
milliseconds({N, seconds}) -> milliseconds({N * 1000, milliseconds});
milliseconds({N, millisecond}) -> to:integer(N);
milliseconds({N, milliseconds}) -> to:integer(N);
milliseconds(V) -> to:integer(V).

seconds({N, day}) -> seconds({N * 24, hours});
seconds({N, days}) -> seconds({N * 24, hours});
seconds({N, hour}) -> seconds({N * 60, minutes});
seconds({N, hours}) -> seconds({N * 60, minutes});
seconds({N, minute}) -> seconds({N * 60, seconds});
seconds({N, minutes}) -> seconds({N * 60, seconds});
seconds({N, second}) -> to:integer(N);
seconds({N, seconds}) -> to:integer(N);
seconds({N, millisecond}) -> seconds({N / 1000, seconds});
seconds({N, milliseconds}) -> seconds({N / 1000, seconds});
seconds(V) -> to:integer(V).
