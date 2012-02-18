-module(to).
-export([atom/1, integer/1, string/1]).
-ignore_xref([atom/1, integer/1, string/1]).

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
