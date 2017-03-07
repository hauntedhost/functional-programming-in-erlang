-module(list).
-export([
  head/1,
  maximum/1,
  product/1,
  sum/1,
  sum_t/1,
  tail/1,
  weird_case/1
]).

head([X | _Xs]) -> X.
tail([_X | Xs]) -> Xs.

maximum([X | Xs]) -> maximum(Xs, X).
maximum([], M) -> M;
maximum([X | Xs], M) when X > M -> maximum(Xs, X);
maximum([_X | Xs], M) -> maximum(Xs, M).

product([X | Xs]) -> product(Xs, X).
product([], P) -> P;
product([X | Xs], P) -> product(Xs, P * X).

% direct recursive sum
sum([]) -> 0;
sum([X | Xs]) -> X + sum(Xs).

% tail recursive sum
sum_t(L) -> sum_t(L, 0).
sum_t([], S) -> S;
sum_t([X | Xs], S) -> sum_t(Xs, X + S).

weird_case(L) when is_list(L) ->
  case L of
    [A, B, _] -> A + B;
    [A, B]    -> A * B;
    [A]       -> A;
    _         -> 0
  end.
