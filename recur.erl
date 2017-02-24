-module(recur).
-export([
  fac/1,
  fib/1,
  fibs1/1,
  fibs2/1,
  pieces/1,
  tail_fac/1,
  tail_fib/1,
  tail_fibs/1,
  tail_pieces/1
]).

fac(0)            -> 1;
fac(N) when N > 0 -> fac(N - 1) * N.

tail_fac(N)                 -> tail_fac(N, 1).
tail_fac(0, Acc)            -> Acc;
tail_fac(N, Acc) when N > 0 -> tail_fac(N - 1, N * Acc).

fib(0) -> nil;
fib(1) -> 0;
fib(2) -> 1;
fib(N) -> fib(N - 2) + fib(N - 1).

tail_fib(N)       -> tail_fib(N, 0, 1).
tail_fib(0, _, _) -> nil;
tail_fib(1, Y, _) -> Y;
tail_fib(2, _, Z) -> Z;
tail_fib(N, Y, Z) -> tail_fib(N - 1, Z, Y + Z).

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

tail_fibs(N) ->
  tail_fibs(N, [0, 1, 1]).
tail_fibs(3, Fibs) ->
  Fibs;
tail_fibs(N, Fibs) when N < 3 ->
  lists:sublist(Fibs, N);
tail_fibs(N, Fibs) when N > 3 ->
  [Y, Z] = lists:nthtail(length(Fibs) - 2, Fibs),
  tail_fibs(N - 1, Fibs ++ [Y + Z]).

pieces(0) -> 0;
pieces(1) -> 2;
pieces(N) -> N + pieces(N - 1).

tail_pieces(N)    -> tail_pieces(N, 2).
tail_pieces(0, _) -> 0;
tail_pieces(1, P) -> P;
tail_pieces(N, P) -> tail_pieces(N - 1, N + P).
