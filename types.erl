-module(types).
-export([
  numbers/0,
  atoms/0,
  booleans/0,
  strings/0,
  tuples/0,
  lists/0,
  functions/0
]).

% numbers
numbers() ->
  A = 5,
  B = A * 3,
  B div 2.

% atoms
atoms() ->
  foo,
  bar == bar.

% booleans (are atoms)
booleans() ->
  false,
  true == true.

% strings
strings() ->
  S = "foo",
  S ++ "bar".

% tuples
tuples() ->
  T = {3, 12, 9},
  {triangle, T}.

% lists
lists() ->
  L = ["strange", "world"],
  ["hello" | L].

% functions
functions() ->
  S = fun(X) -> X * X end,
  lists:map(S, [2, 3, 4]).
