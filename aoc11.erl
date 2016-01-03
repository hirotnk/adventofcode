-module(aoc11).
-export([start/0]).

start() ->
  {get_next("cqjxjnds"), get_next("cqjxxyzz")}.

get_next(Pass0) ->
  Pass1 = lists:reverse(increment(lists:reverse(Pass0))),
  case has_increasing_3letters(Pass1) andalso (not has_iol(Pass1)) andalso has_2doubles(Pass1, 0) of
    true -> Pass1;
    false -> get_next(Pass1)
  end.

increment([$z]) -> [$a];
increment([]) -> [];
increment([$z|T]) -> [$a|increment(T)];
increment([H|T]) -> [H+1|T].

has_increasing_3letters([]) -> false;
has_increasing_3letters([A,B,C|_]) when (C - B) == 1 andalso (B - A) == 1 -> true;
has_increasing_3letters([_|T]) ->
  has_increasing_3letters(T).

has_iol(S) ->
  lists:member($i, S) orelse
  lists:member($o, S) orelse
  lists:member($l, S).

has_2doubles([],_Cnt) -> false;
has_2doubles([A,A|T],0) -> has_2doubles(T,1);
has_2doubles([A,A|_],1) -> true;
has_2doubles([_|T],Cnt) -> has_2doubles(T,Cnt).



%   --- Day 11: Corporate Policy ---
%   
%   Santa's previous password expired, and he needs help choosing a new one.
%   
%   To help him remember his new password after the old one expires, Santa has
%   devised a method of coming up with a password based on the previous one.
%   Corporate policy dictates that passwords must be exactly eight lowercase
%   letters (for security reasons), so he finds his new password by incrementing
%   his old password string repeatedly until it is valid.
%   
%   Incrementing is just like counting with numbers: xx, xy, xz, ya, yb, and so on.
%   Increase the rightmost letter one step; if it was z, it wraps around to a, and
%   repeat with the next letter to the left until one doesn't wrap around.
%   
%   Unfortunately for Santa, a new Security-Elf recently started, and he has
%   imposed some additional password requirements:
%   
%   Passwords must include one increasing straight of at least three letters, like
%   abc, bcd, cde, and so on, up to xyz. They cannot skip letters; abd doesn't
%   count.  Passwords may not contain the letters i, o, or l, as these letters can
%   be mistaken for other characters and are therefore confusing.  Passwords must
%   contain at least two different, non-overlapping pairs of letters, like aa, bb,
%   or zz.  For example:
%   
%   hijklmmn meets the first requirement (because it contains the straight hij) but
%   fails the second requirement requirement (because it contains i and l).
%   abbceffg meets the third requirement (because it repeats bb and ff) but fails
%   the first requirement.  abbcegjk fails the third requirement, because it only
%   has one double letter (bb).  The next password after abcdefgh is abcdffaa.  The
%   next password after ghijklmn is ghjaabcc, because you eventually skip all the
%   passwords that start with ghi..., since i is not allowed.  Given Santa's
%   current password (your puzzle input), what should his next password be?
%   
%   Your puzzle answer was cqjxxyzz.
%   
%   --- Part Two ---
%   
%   Santa's password expired again. What's the next one?
%   
%   Your puzzle answer was cqkaabcc.
%   
%   Both parts of this puzzle are complete! They provide two gold stars: **
%   
%   At this point, all that is left is for you to admire your advent calendar.
%   
%   Your puzzle input was cqjxjnds.
%   
%   You can also [Share] this puzzle.
%   
