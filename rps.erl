-module(rps).
-export([
  match/2,
  play/2
]).
-import(hof, [
  map/2,
  reduce/3,
  zip_with/3
]).

% Rock defeats scissors, because a rock will blunt a pair of scissors
% Paper defeats rock, because a paper can wrap up a rock
% Scissors defeat paper, because scissors cut paper

what_beats(rock)     -> paper;
what_beats(paper)    -> scissors;
what_beats(scissors) -> rock;
what_beats(_)        -> unknown.

what_loses_to(rock)     -> scissors;
what_loses_to(paper)    -> rock;
what_loses_to(scissors) -> paper;
what_loses_to(_)        -> unknown.

score(lose) -> -1;
score(win)  -> 1;
score(draw) -> 0.

% play one round

play(A, B) ->
  case {what_beats(A), what_loses_to(A)} of
    {B, _}  -> lose;
    {_, B}  -> win;
    {_ , _} -> draw
  end.

% play multiple rounds

% rps:match([rock, rock, rock, rock], [scissors, scissors, scissors, paper]) == 2.

% using tail optimized recursion -- more efficient
match(Xs, Ys) -> match(Xs, Ys, 0).
match([X | Xs], [Y | Ys], N) -> match(Xs, Ys, score(play(X, Y)) + N);
match(_, _, N) -> N.

% match using zip_with and reduce
% match(Xs, Ys) ->
%   Results = zip_with(fun play/2, Xs, Ys),
%   Scores = map(fun score/1, Results),
%   reduce(fun (N, Sum) -> Sum + N end, 0, Scores).

% stategies

enum(0) -> rock;
enum(1) -> paper;
enum(2) -> scissors;

value(rock)     -> 0;
value(paper)    -> 1;
value(scissors) -> 2.
