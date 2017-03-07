-module(week1).
-export([
  area/1,
  bits_b/1,
  bits_n/1,
  enclose/1,
  perimeter/1
]).

perimeter({square, S}) ->
  4 * S;
perimeter({rectangle, {W, L}}) ->
  2 * (W + L);
perimeter({rectangle, #{width := W, length := L}}) ->
  2 * (W + L);
perimeter({triangle, S}) when is_number(S) ->
  3 * S;
perimeter({triangle, {A, B, C}}) ->
  A + B + C;
perimeter({circle, R}) ->
  2 * math:pi() * R.

area({square, S}) ->
  S * S;
area({rectangle, {W, L}}) ->
  W * L;
area({rectangle, #{width := W, length := L}}) ->
  W * L;
% heron's formula
area({triangle, {X, Y, Z}}) ->
  [C, B, A] = lists:sort([X, Y, Z]),
  P = (A + B + C) / 2,
  math:sqrt(P * (P - A) * (P - B) * (P - C));
area({triangle, #{base := B, height := H}}) ->
  B * H / 2;
area({circle, R}) ->
  math:pi() * (R * R).

enclose({square, S}) ->
  {rectangle, {S, S}};
enclose(R = {rectangle, _}) ->
  R;
enclose({triangle, #{base := B, height := H}}) ->
  {rectangle, {B, H}};
enclose({circle, R}) ->
  D = R * 2,
  {rectangle, {D, D}}.

% bits with tail recursion
bits_n(N) ->
  bits_n(N, 0).

bits_n(N, R) when N < 2, N >= 0 ->
  R;
bits_n(N, R) ->
  bits_n(N div 2, R + N rem 2).

% tail recursive using binary conversion/matching (maybe weird)
bits_b(N) when is_number(N) ->
  bits_b(integer_to_binary(N, 2), 0).

bits_b(<<>>, S) ->
  S;
bits_b(<<H:1/binary, T/binary>>, S) ->
  bits_b(T, S + binary_to_integer(H)).
