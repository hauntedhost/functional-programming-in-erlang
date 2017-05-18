-module (hof).
-export([
  filter/2,
  evens/1,
  odds/1,
  % ---
  map/2,
  doubles/1,
  squares/1
]).

filter(F, List) -> filter(F, List, []).
filter(_, [], Accum) -> lists:reverse(Accum);
filter(F, [X | Xs], Accum) ->
  case F(X) of
    true  -> filter(F, Xs, [X | Accum]);
    false -> filter(F, Xs, Accum)
  end.

map(F, List) -> map(F, List, []).
map(_, [], Accum) -> lists:reverse(Accum);
map(F, [X | Xs], Accum) -> map(F, Xs, [F(X) | Accum]).

is_even(N) -> N rem 2 == 0.
is_odd(N) -> not(is_even(N)).

evens(List) -> filter(fun is_even/1, List).
odds(List) -> filter(fun is_odd/1, List).

doubles(List) -> map(fun(N) -> N * 2 end, List).
squares(List) -> map(fun(N) -> N * N end, List).
