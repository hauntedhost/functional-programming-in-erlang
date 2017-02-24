-module(tail).
-export([
  fac/1,
  fib/1,
  fibs1/1,
  fibs2/1,
  flatten/1,
  perfect1/1,
  perfect2/1,
  pieces/1
]).

% --

fac(N) ->
  fac(N, 1).

fac(0, F) ->
  F;
fac(N, F) when N > 0 ->
  fac(N - 1, N * F).

% --

fib(N) ->
  fib(N, 0, 1).

fib(0, _, _) ->
  nil;
fib(1, Y, _) ->
  Y;
fib(N, Y, Z) ->
  fib(N - 1, Z, Y + Z).

% --

fibs1(N) ->
  fibs1(N, [0, 1, 1]).

fibs1(3, Fibs) ->
  Fibs;
fibs1(N, Fibs) when N < 3 ->
  lists:sublist(Fibs, N);
fibs1(N, Fibs) when N > 3 ->
  [Y, Z] = lists:nthtail(length(Fibs) - 2, Fibs),
  fibs1(N - 1, Fibs ++ [Y + Z]).

% return list of n fibs, reversing final list just once

fibs2(N) ->
  fibs2(N, [1, 1, 0]).

fibs2(3, Fibs) ->
  lists:reverse(Fibs);
fibs2(N, Fibs) when N < 3 ->
  lists:sublist(lists:reverse(Fibs), N);
fibs2(N, Fibs = [Z, Y | _]) when N > 3 ->
  fibs2(N - 1, [Y + Z | Fibs]).

% --

pieces(N) ->
  pieces(N, 2).

pieces(0, _) ->
  0;
pieces(1, P) ->
  P;
pieces(N, P) ->
  pieces(N - 1, N + P).

% returns a boolean which indicates whether or not the number is perfect

perfect1(N) ->
  perfect1(N, _Sum = 0, _Div = 1).

perfect1(N, Sum, Div) when Div == N ->
  Sum == N;
perfect1(N, Sum, Div) when N rem Div == 0 ->
  perfect1(N, Sum + Div, Div + 1);
perfect1(N, Sum, Div) ->
  perfect1(N, Sum, Div + 1).

perfect2(N) ->
  perfect2(N, _Div = 1, _Sum = 0).

perfect2(N, _Div = N, Sum) ->
  N == Sum;
perfect2(N, Div, Sum) when N rem Div == 0 ->
  perfect2(N, Div + 1, Sum + Div);
perfect2(N, Div, Sum) ->
  perfect2(N, Div + 1, Sum).

% hacker news inspired me today
% https://news.ycombinator.com/item?id=13723356

flatten(L) ->
  flatten(L, []).

flatten([], F) ->
  lists:reverse(F);
flatten([H | T], F) when is_list(H) ->
  flatten(T ++ H, F);
flatten([H | T], F) ->
  flatten(T, [H | F]).
