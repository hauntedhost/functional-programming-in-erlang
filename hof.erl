-module (hof).
-export([
  filter/2,
  evens/1,
  odds/1,
  % ---
  map/2,
  doubles/1,
  squares/1,
  % ---
  reduce/3,
  group_by/2,
  sum/1,
  word_lengths/1
]).

% filter

filter(F, List) -> filter(F, List, []).
filter(_, [], Accum) -> lists:reverse(Accum);
filter(F, [X | Xs], Accum) ->
  case F(X) of
    true  -> filter(F, Xs, [X | Accum]);
    false -> filter(F, Xs, Accum)
  end.

is_even(N) -> N rem 2 == 0.
is_odd(N) -> not(is_even(N)).

evens(List) -> filter(fun is_even/1, List).
odds(List) -> filter(fun is_odd/1, List).

% map

map(F, List) -> map(F, List, []).
map(_, [], Accum) -> lists:reverse(Accum);
map(F, [X | Xs], Accum) -> map(F, Xs, [F(X) | Accum]).

doubles(List) -> map(fun(N) -> N * 2 end, List).
squares(List) -> map(fun(N) -> N * N end, List).

% reduce

reduce(F, Init, List) -> do_reduce(F, List, Init).
do_reduce(_, [], Accum) -> Accum;
do_reduce(F, [X | Xs], Accum) -> do_reduce(F, Xs, F(X, Accum)).

sum(List) -> reduce(fun(N, Sum) -> N + Sum end, 0, List).

group_by(F, List) -> reduce(
  fun(X, Accum) ->
    Key = F(X),
    case Accum of
      #{Key := Values} -> maps:put(Key, Values ++ [X], Accum);
      _                -> maps:put(Key, [X], Accum)
    end
  end, #{}, List).

word_lengths(Words) -> group_by(fun(W) -> length(W) end, Words).
