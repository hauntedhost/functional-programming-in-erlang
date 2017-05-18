-module (hof).
-export([
  doubles/1,
  map/2
]).

map(F, List) -> map(F, List, []).
map(_, [], Accum) -> lists:reverse(Accum);
map(F, [X | Xs], Accum) -> map(F, Xs, [F(X) | Accum]).

doubles(List) -> map(fun(N) -> N * 2 end, List).
squares(List) -> map(fun(N) -> N * N end, List).
