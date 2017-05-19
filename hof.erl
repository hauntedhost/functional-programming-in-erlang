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
  product/1,
  sum/1,
  word_lengths/1,
  % ---
  zip/2,
  zip_with/3
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

product(List) -> reduce(fun(N, P) -> N * P end, 1, List).

zip_with(F, Xs, Ys) -> zip_with(F, Xs, Ys, []).
zip_with(F, [X | Xs], [Y | Ys], Zs) ->
  Z = F(X, Y),
  zip_with(F, Xs, Ys, [Z | Zs]);
zip_with(_, _, _, Zs) -> lists:reverse(Zs).

zip(Xs, Ys) -> zip_with(fun(X, Y) -> {X, Y} end, Xs, Ys).
% zip(Xs, Ys) -> zip(Xs, Ys, []).
% zip([X | Xs], [Y | Ys], Zs) -> zip(Xs, Ys, [{X, Y} | Zs]);
% zip(_, _, Zs) -> lists:reverse(Zs).
