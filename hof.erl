-module (hof).
-export([
  add_five/1,
  add_five_then_is_ten/1,
  add_ten/1,
  add_twenty/1,
  compose/2,
  doubles/1, % map
  evens/1, % filter
  filter/2, % **
  group_by/2, % map
  is_ten_then_add_five/1,
  iterate/1,
  map/2, % **
  odds/1, % filter
  product/1, % map
  reduce/3, % **
  squares/1, % map
  sum/1, % map,
  twice/1,
  word_lengths/1, % map
  zip_with/3,
  zip/2
]).

compose(F, G) -> fun(X) -> G(F(X)) end.
% compose(List)?

create_adder(X) -> fun(Y) -> X + Y end.
create_is_equal(X) -> fun(Y) -> X == Y end.

add_five_then_is_ten(X) ->
  F = compose(create_adder(5), create_is_equal(10)),
  F(X).

% commutative error
is_ten_then_add_five(X) ->
  F = compose(create_is_equal(10), create_adder(5)),
  F(X).

twice(F) -> fun(X) -> F(F(X)) end.

% let's overcomplicate adding numbers for fun
add_five(X) -> (create_adder(5))(X).
add_ten(X) -> (twice(fun add_five/1))(X).
add_twenty(X) -> (twice(twice(fun add_five/1)))(X).

iterate(0) -> fun(F) -> F end;
iterate(N) -> fun(F) -> F(iterate(N - 1)) end.

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
