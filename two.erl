-module(two).
-export([area/2, hyp/2, perim/2]).

% square a number
square(X) ->
  X * X.

% calculate hypotenuse of right triangle
hyp(A, B) ->
  math:sqrt(square(A) + square(B)).

% calculate perimeter of right triangle
perim(A, B) ->
  (A + B) + hyp(A, B).

% calculate area of right triangle
area(A, B) ->
  (A * B) / 2.
