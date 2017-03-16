-module(shapes).
-export([circles/1, circles_t/1]).

circles([]) -> [];
circles([C = {circle, {_, _} , _} | Xs]) -> [C | circles(Xs)];
circles([_ | Xs]) -> circles(Xs).

% tail recursive circles
circles_t(Xs) -> circles_t(Xs, []).

circles_t([], Cs) ->
  Cs;
circles_t([C = {circle, {_, _} , _} | Xs], Cs) ->
  circles_t(Xs, [C | Cs]);
circles_t([_ | Xs], Cs) ->
  circles_t(Xs, Cs).
