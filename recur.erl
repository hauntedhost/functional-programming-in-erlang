-module(recur).
-export([
  fac/1,
  fib/1,
  fibs1/1,
  fibs2/1,
  flatten/1,
  pieces/1
]).

fac(0) ->
  1;
fac(N) when N > 0 ->
  fac(N - 1) * N.

fib(0) ->
  nil;
fib(1) ->
  0;
fib(2) ->
  1;
fib(N) ->
  fib(N - 2) + fib(N - 1).

fibs1(0) ->
  [];
fibs1(1) ->
  [0];
fibs1(2) ->
  [0, 1];
fibs1(3) ->
  [0, 1, 1];
fibs1(N) when N > 3 ->
  Fibs = fibs1(N - 1),
  [Y, Z] = lists:nthtail(length(Fibs) - 2, Fibs),
  Fibs ++ [Y + Z].

fibs2(3) ->
  [0, 1, 1];
fibs2(N) when N < 3 ->
  lists:sublist(fibs2(3), N);
fibs2(N) when N > 3 ->
  Fibs = fibs2(N - 1),
  [Y, Z] = lists:nthtail(length(Fibs) - 2, Fibs),
  Fibs ++ [Y + Z].

pieces(0) ->
  0;
pieces(1) ->
  2;
pieces(N) ->
  N + pieces(N - 1).

% hacker news inspired me today
% https://news.ycombinator.com/item?id=13723356

flatten([]) ->
  [];
flatten([X | Xs]) when is_list(X) ->
  flatten(X) ++ flatten(Xs);
flatten([X | Xs]) ->
  [X | flatten(Xs)].
