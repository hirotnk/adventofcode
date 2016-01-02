-module(aoc05).
-export([start/0]).

start() ->
  get_data_list().

get_data_list() ->
  {ok, Bin} = file:read_file("aoc05.txt"),
  length([B || B <- binary:split(Bin, [<<"\n">>], [global]), is_good2(B)]).

is_good2(B) ->
  S = binary_to_list(B),
  is_oneafterone(S) andalso 
  is_2twice(S).

is_oneafterone([]) -> false;
is_oneafterone([_H]) -> false;
is_oneafterone([A,B,A|T]) -> true;
is_oneafterone([H|T]) ->
  is_oneafterone(T).

is_2twice([]) -> false;
is_2twice([_H]) -> false;
is_2twice([A,B|T]) ->
  case string:str(T, [A,B]) of
    0 -> is_2twice([B|T]);
    _ -> true
  end.

is_good(B) ->
  S = binary_to_list(B),
  check_forbiddens(S) andalso 
  is_valid_vowels(S) andalso
  is_double_chars(S).

check_forbiddens(S) ->
  lists:all(fun(E) -> string:str(S, E) == 0 end, ["ab", "cd", "pq", "xy"]).

is_valid_vowels(S) ->
  lists:foldl(
    fun(E,Cnt) -> 
      if
        E == $a orelse E== $i orelse E == $u orelse E == $e orelse E == $o -> Cnt + 1;
        true -> Cnt
      end
    end, 0, S) >= 3.
    

is_double_chars([]) -> false;
is_double_chars([_H,_H|_T]) -> true;
is_double_chars([_H|T]) -> 
  is_double_chars(T).


%--- Day 5: Doesn't He Have Intern-Elves For This? ---
%
%Santa needs help figuring out which strings in his text file are naughty or nice.
%
%A nice string is one with all of the following properties:
%
%It contains at least three vowels (aeiou only), like aei, xazegov, or aeiouaeiouaeiou.
%It contains at least one letter that appears twice in a row, like xx, abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
%It does not contain the strings ab, cd, pq, or xy, even if they are part of one of the other requirements.
%For example:
%
%ugknbfddgicrmopn is nice because it has at least three vowels (u...i...o...), a double letter (...dd...), and none of the disallowed substrings.
%aaa is nice because it has at least three vowels and a double letter, even though the letters used by different rules overlap.
%jchzalrnumimnmhp is naughty because it has no double letter.
%haegwjzuvuyypxyu is naughty because it contains the string xy.
%dvszwmarrgswjxmb is naughty because it contains only one vowel.
%How many strings are nice?
%
%Your puzzle answer was 236.
%
%--- Part Two ---
%
%Realizing the error of his ways, Santa has switched to a better model of determining whether a string is naughty or nice. None of the old rules apply, as they are all clearly ridiculous.
%
%Now, a nice string is one with all of the following properties:
%
%It contains a pair of any two letters that appears at least twice in the string without overlapping, like xyxy (xy) or aabcdefgaa (aa), but not like aaa (aa, but it overlaps).
%It contains at least one letter which repeats with exactly one letter between them, like xyx, abcdefeghi (efe), or even aaa.
%For example:
%
%qjhvhtzxzqqjkmpb is nice because is has a pair that appears twice (qj) and a letter that repeats with exactly one letter between them (zxz).
%xxyxx is nice because it has a pair that appears twice and a letter that repeats with one between, even though the letters used by each rule overlap.
%uurcxstgmygtbstg is naughty because it has a pair (tg) but no repeat with a single letter between them.
%ieodomkazucvgmuy is naughty because it has a repeating letter with one between (odo), but no pair that appears twice.
%How many strings are nice under these new rules?
%
%Your puzzle answer was 51.
%
%Both parts of this puzzle are complete! They provide two gold stars: **
%
%At this point, all that is left is for you to admire your advent calendar.
%
%If you still want to see it, you can get your puzzle input.
%
%You can also [Share] this puzzle.
%
