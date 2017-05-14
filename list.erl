-module(list).
-export([
  concat/1,
  double/1,
  evens/1,
  filter/2,
  head/1,
  is_member/2,
  is_palindrome/1,
  join/2,
  maximum/1,
  median/1,
  modes/1,
  nub_first/1,
  nub_last/1,
  product/1,
  remove_all/2,
  remove_punctuation/1,
  remove_punctuation2/1,
  reverse/1,
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
take_t(0, _, Accum) -> reverse(Accum);
take_t(_N, [], Accum) -> reverse(Accum);
take_t(N, [X | Xs], Accum) when N > 0 ->
  take_t(N - 1, Xs, [X | Accum]).

% given integer and list, returns tuple with
% slice of N elements on left and remainder on right
-spec split_t(integer(), [T]) -> {[T], [T]}.
split_t(N, Right) -> split_t(N, [], Right).
split_t(0, Left, Right) -> {reverse(Left), Right};
split_t(N, Left, [X | Xs]) ->
  split_t(N - 1, [X | Left], Xs);
split_t(_N, _Left, []) ->
  erlang:error(badarg).

% remove dups from list, keeping first occurence
nub_first(List) -> nub_first(List, [], #{}).
nub_first([], Accum, _Cache) -> reverse(Accum);
nub_first([X | Xs], Accum, Cache) ->
  case Cache of
    #{X := _} -> nub_first(Xs, Accum, Cache);
    _         -> nub_first(Xs, [X | Accum], maps:put(X, true, Cache))
  end.

% remove dups from list, keeping last occurence
nub_last(List) ->
  Result = nub_first(reverse(List)),
  reverse(Result).

% check if value is in list
is_member(_X, []) -> false;
is_member(X, [X | _Xs]) -> true;
is_member(X, [_Y | Xs]) -> is_member(X, Xs).

% remove all occurences of value from list
remove_all(X, List) -> remove_all(X, List, []).
remove_all(_X, [], Accum) -> reverse(Accum);
remove_all(X, [X | Xs], Accum) -> remove_all(X, Xs, Accum);
remove_all(X, [Y | Xs], Accum) -> remove_all(X, Xs, [Y | Accum]).

% check if given string is a palindrom
is_palindrome(String) ->
  Word = remove_punctuation(String),
  Reversed = reverse(Word),
  string:to_lower(Word) == string:to_lower(Reversed).

% strip punctuation, leveraging string:tokens
remove_punctuation(String) ->
  Punctuation = " !@#$%^&*()-_=+{}[]|\\;:'\"<>,.?/\n\t",
  Tokens = string:tokens(String, Punctuation),
  string:join(Tokens, "").

% strip punctuation recursively
remove_punctuation2(List) -> remove_punctuation2(List, []).
remove_punctuation2([], Accum) -> reverse(Accum);
remove_punctuation2([X | Xs], Accum) ->
  Punctuation = " !@#$%^&*()-_=+{}[]|\\;:'\"<>,.?/\n\t",
  case is_member(X, Punctuation) of
    true  -> remove_punctuation2(Xs, Accum);
    false -> remove_punctuation2(Xs, [X | Accum])
  end.

% filter list using given function
filter(List, PredicateFun) -> filter(List, PredicateFun, []).
filter([], _PredicateFun, Accum) -> reverse(Accum);
filter([X | Xs], PredicateFun, Accum) ->
  case PredicateFun(X) of
    true  -> filter(Xs, PredicateFun, [X | Accum]);
    false -> filter(Xs, PredicateFun, Accum)
  end.

% reverse given list
reverse(List) -> reverse(List, []).
reverse([], Reversed) -> Reversed;
reverse([X | Xs], Reversed) -> reverse(Xs, [X | Reversed]).

% join two lists together
join(Xs, Ys) -> do_join(reverse(Xs), Ys).
do_join([], Accum) -> Accum;
do_join([X | Xs], Accum) -> do_join(Xs, [X | Accum]).

% concat a list of lists
% e.g. concat([[1, 2], [3, 4]]) = [1, 2, 3, 4].
concat(List) -> concat(List, []).
concat([], Accum) -> Accum;
concat([Xs | Ys], Accum) -> concat(Ys, join(Accum, Xs)).

% TODO: Sorting lists
% A list can be sorted in a number of ways, including these algorithms described informally:
%
% Merge sort: divide the list into two halves of (approximately) equal length, sort them (recursively) and then merge the results.
%
% Quicksort: split the list into two according to whether the items are smaller than (or equal to) or larger than the pivot, often taken to be the head element of the list; sort the two halves and join the results together.
%
% Insertion sort: sort the tail of the list and then insert the head of the list in the correct place.

% TODO: Permutations
% A permutation of a list xs consists of the same elements in a (potentially) different order. Define a function that gives all the permutations of a list, in some order. For example:
%
% perms([]) = [[]]
% perms([1,2,3]) = [[1,2,3],[2,3,1],[3,1,2],[2,1,3],[1,3,2],[3,2,1]]
