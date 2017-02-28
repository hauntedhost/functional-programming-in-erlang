-module(week1).
-export([
  area/1,
  bits/1,
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

% tail recursive
bits(N) when is_number(N) ->
  bits(integer_to_binary(N, 2), 0).

bits(<<>>, S) ->
  S;
bits(<<H:1/binary, T/binary>>, S) ->
  bits(T, S + binary_to_integer(H)).
