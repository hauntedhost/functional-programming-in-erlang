-module(first).
-export([area/3, double/1, mult/2]).

mult(X, Y) ->
  X * Y.

double(X) ->
  mult(2, X).

area(A, B, C) ->
  S = (A + B + C) / 2,
  math:sqrt(S * (S - A) * (S - B) * (S - C)).
