-module(patterns1).
-export([
  how_many_equal/3,
  is_zero/1,
  max_three/3,
  xor1/2,
  xor2/2,
  xor3/2,
  xor4/2,
  xor5/2,
  xor6/2
]).

is_zero(0) ->
  true;
is_zero(_) ->
  false.

xor1(true, false) ->
  true;
xor1(false, true) ->
  true;
xor1(_, _) ->
  false.

xor2(X, X) ->
  false;
xor2(_, _) ->
  true.

xor3(X, Y) ->
  X =/= Y.

xor4(X, Y) when is_boolean(X), is_boolean(Y) ->
  (X and not(Y)) or (Y and not(X)).

xor5(X, true) when is_boolean(X) ->
  not(X);
xor5(X, false) when is_boolean(X) ->
  X.

xor6(true, X) when is_boolean(X) ->
  not(X);
xor6(false, X) when is_boolean(X) ->
  X.

max_three(A, B, C) ->
  max(max(A, B), C).

how_many_equal(A, A, A) ->
  3;
how_many_equal(_, A, A) ->
  2;
how_many_equal(A, _, A) ->
  2;
how_many_equal(A, A, _) ->
  2;
how_many_equal(_, _, _) ->
  0.
