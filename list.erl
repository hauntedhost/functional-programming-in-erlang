-module(list).
-export([
  double/1,
  evens/1,
  head/1,
  maximum/1,
  median/1,
  modes/1,
  nub/1,
  product/1,
  split_t/2,
  sum/1,
  sum_t/1,
  tail/1,
  take/2,
  take_t/2,
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

double([]) -> [];
double([X | Xs]) -> [X * 2 | double(Xs)].

evens([]) -> [];
evens([X | Xs]) when X rem 2 == 0 -> [X | evens(Xs)];
evens([_ | Xs]) -> evens(Xs).

median(L = [_ | _]) -> do_median(lists:sort(L)).

do_median([M]) ->
  M;
do_median(L) when length(L) rem 2 == 0 ->
  NthA = length(L) div 2,
  NthB = NthA + 1,
  (lists:nth(NthA, L) + lists:nth(NthB, L)) / 2;
do_median(L) ->
  Nth = (length(L) div 2) + 1,
  lists:nth(Nth, L).

% given list, returns map with counts of how many times each value occured
modes(List) -> modes(List, #{}).
modes([], Modes) -> Modes;
modes([X | Xs], Modes) ->
  Count = maps:get(X, Modes, 0) + 1,
  modes(Xs, maps:put(X, Count, Modes)).

% given integer and list, returns slice of N elements
-spec take(integer(), [T]) -> [T].
take(0, _) -> [];
take(_N, []) -> [];
take(N, [X | Xs]) when N > 0 -> [X | take(N - 1, Xs)].

% tail call optimized take
-spec take_t(integer(), [T]) -> [T].
take_t(N, List) -> take_t(N, List, []).
take_t(0, _, Accum) -> lists:reverse(Accum);
take_t(_N, [], Accum) -> lists:reverse(Accum);
take_t(N, [X | Xs], Accum) when N > 0 ->
  take_t(N - 1, Xs, [X | Accum]).

% given integer and list, returns tuple with
% slice of N elements on left and remainder on right
-spec split_t(integer(), [T]) -> {[T], [T]}.
split_t(N, Right) -> split_t(N, [], Right).
split_t(0, Left, Right) -> {lists:reverse(Left), Right};
split_t(N, Left, [X | Xs]) ->
  split_t(N - 1, [X | Left], Xs);
split_t(_N, _Left, []) ->
  erlang:error(badarg).

% remove dups from list
nub(List) -> nub(List, [], #{}).
nub([], Accum, _Cache) -> lists:reverse(Accum);
nub([X | Xs], Accum, Cache) ->
  case Cache of
    #{X := _} -> nub(Xs, Accum, Cache);
    _         -> nub(Xs, [X | Accum], maps:put(X, true, Cache))
  end.
