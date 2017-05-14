-module(word_stats).
-export([
  process_file/1,
  show_stats/1
]).

% use to display large Stats result from process_file/1
show_stats(Stats) -> io:format("~p~n", [Stats]).

% process words on each line of given file and return map where:
% - keys are every word found
% - values are list of line numbers where word was found
%
% example:
%   #{"foo" => [1, 15, 22], "bar" => [2], "baz" => [1, 3, 19]}
%
process_file(Filename) ->
  Lines = get_file_lines(Filename),
  process_file(Lines, 1, #{}).
process_file([], _LineNum, Stats) -> Stats;
process_file([Line | Lines], LineNum, Stats) ->
  Words = process_line_to_words(Line),
  NewStats = process_words(Words, LineNum, Stats),
  process_file(Lines, LineNum + 1, NewStats).

% split line into clean words at least 4 characters in length
process_line_to_words(Line) ->
  CleanLine = clean_line(Line),
  AllWords = string:tokens(CleanLine, " "),
  lists:filter(fun(Word) -> length(Word) >= 4 end, AllWords).

% trim trailing whitespace, lowercase, strip punctuation
clean_line(Line) ->
  strip_punctuation(string:to_lower(string:strip(Line))).

% strip punctuation
strip_punctuation(String) ->
  Punctuation = "`!@#$%^&*()-_=+{}[]|\\;:'\"<>,.?/\n\t",
  Tokens = string:tokens(String, Punctuation),
  string:join(Tokens, "").

% update stats map key/values for each word
process_words([], _LineNum, Stats) -> Stats;
process_words([Word | Words], LineNum, Stats) ->
  case Stats of
    #{Word := LineNums} ->
      NewStats = maps:put(Word, add_unique(LineNum, LineNums), Stats),
      process_words(Words, LineNum, NewStats);
    _ ->
      NewStats = maps:put(Word, [LineNum], Stats),
      process_words(Words, LineNum, NewStats)
  end.

% add value to list if not already present
add_unique(Value, List) ->
  case lists:member(Value, List) of
    true -> List;
    false -> List ++ [Value]
  end.

% Used to read a file into a list of lines.
% Example files available in:
%   gettysburg-address.txt (short)
%   dickens-christmas.txt  (long)

% Get the contents of a text file into a list of lines.
% Each line has its trailing newline removed.

get_file_lines(Filename) ->
  {ok, File} = file:open(Filename, [read]),
  Lines = get_file_lines(File, []),
  lists:reverse(Lines).

get_file_lines(File, Lines) ->
  case io:get_line(File, "") of
    eof ->
      file:close(File),
      Lines;
    Line ->
      {Strip, _} = lists:split(length(Line) - 1, Line),
      get_file_lines(File, [Strip | Lines])
  end.

% Show the contents of a list of strings.
% Can be used to check the results of calling get_file_contents.

show_file_contents([]) -> ok;
show_file_contents([L | Ls]) ->
  io:format("~s~n", [L]),
  show_file_contents(Ls).
