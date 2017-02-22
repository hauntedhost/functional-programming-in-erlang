-module(first).
-export([area/3, double/1, mult/2]).

% calculate hypotenuse of right triangle
mult(X, Y) ->
  X * Y.

% double a number
double(X) ->
  mult(2, X).

% calculate area of a triangle
area(A, B, C) ->
  S = (A + B + C) / 2,
  math:sqrt(S * (S - A) * (S - B) * (S - C)).
