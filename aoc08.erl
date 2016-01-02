-module(aoc08).
-export([start/0]).

start() ->
  get_data_list().

get_data_list() ->
  {ok, Bin} = file:read_file("aoc08.txt"),
  L0 = [len(binary_to_list(B), {0,0})
     || B <- binary:split(Bin, [<<"\n">>], [global]), size(B) > 0],
  L1 = [len2(binary_to_list(B), {0,0})
     || B <- binary:split(Bin, [<<"\n">>], [global]), size(B) > 0],
  F = 
    fun({Clen, Slen}, {Acc0,Acc1}) ->
      {Acc0+Clen, Acc1+Slen}
    end,
  {Ctotal0, Stotal0} = lists:foldl(F, {0,0}, L0),
  {Ctotal1, Stotal1} = lists:foldl(F, {0,0}, L1),
  io:format("part1:~p~n", [Ctotal0 - Stotal0]),
  io:format("part2:~p~n", [Stotal1 - Ctotal1]).

% part 1
len([], {Clen,Slen}) -> {Clen, Slen-2};
len([$\\,$x,A,B|T], {Clen,Slen})
   when ((A >= $0 andalso A =< $9) orelse (A >= $a andalso A =< $f)) andalso
        ((B >= $0 andalso B =< $9) orelse (B >= $a andalso B =< $f)) ->
  len(T, {Clen+4, Slen+1});
len([$\\,$"|T], {Clen,Slen}) -> len(T, {Clen+2, Slen+1});
len([$\\,$\\|T], {Clen,Slen}) -> len(T, {Clen+2, Slen+1});
len([_H|T], {Clen,Slen}) -> len(T, {Clen+1, Slen+1}).

% part2
len2([], {Clen,Slen}) -> {Clen, Slen+4};
len2([$\\,$x,A,B|T], {Clen,Slen})
   when ((A >= $0 andalso A =< $9) orelse (A >= $a andalso A =< $f)) andalso
        ((B >= $0 andalso B =< $9) orelse (B >= $a andalso B =< $f)) ->
  len2(T, {Clen+4, Slen+5});
len2([$\\,$"|T], {Clen,Slen}) -> len2(T, {Clen+2, Slen+4});
len2([$\\,$\\|T], {Clen,Slen}) -> len2(T, {Clen+2, Slen+4});
len2([_H|T], {Clen,Slen}) -> len2(T, {Clen+1, Slen+1}).



%   --- Day 8: Matchsticks ---
%   
%   Space on the sleigh is limited this year, and so Santa will be bringing his list as a digital copy. He needs to know how much space it will take up when stored.
%   
%   It is common in many programming languages to provide a way to escape special characters in strings. For example, C, JavaScript, Perl, Python, and even PHP handle special characters in very similar ways.
%   
%   However, it is important to realize the difference between the number of characters in the code representation of the string literal and the number of characters in the in-memory string itself.
%   
%   For example:
%   
%   "" is 2 characters of code (the two double quotes), but the string contains zero characters.
%   "abc" is 5 characters of code, but 3 characters in the string data.
%   "aaa\"aaa" is 10 characters of code, but the string itself contains six "a" characters and a single, escaped quote character, for a total of 7 characters in the string data.
%   "\x27" is 6 characters of code, but the string itself contains just one - an apostrophe ('), escaped using hexadecimal notation.
%   Santa's list is a file that contains many double-quoted string literals, one on each line. The only escape sequences used are \\ (which represents a single backslash), \" (which represents a lone double-quote character), and \x plus two hexadecimal characters (which represents a single character with that ASCII code).
%   
%   Disregarding the whitespace in the file, what is the number of characters of code for string literals minus the number of characters in memory for the values of the strings in total for the entire file?
%   
%   For example, given the four strings above, the total number of characters of string code (2 + 5 + 10 + 6 = 23) minus the total number of characters in memory for string values (0 + 3 + 7 + 1 = 11) is 23 - 11 = 12.
%   
%   Your puzzle answer was 1350.
%   
%   --- Part Two ---
%   
%   Now, let's go the other way. In addition to finding the number of characters of code, you should now encode each code representation as a new string and find the number of characters of the new encoded representation, including the surrounding double quotes.
%   
%   For example:
%   
%   "" encodes to "\"\"", an increase from 2 characters to 6.
%   "abc" encodes to "\"abc\"", an increase from 5 characters to 9.
%   "aaa\"aaa" encodes to "\"aaa\\\"aaa\"", an increase from 10 characters to 16.
%   "\x27" encodes to "\"\\x27\"", an increase from 6 characters to 11.
%   Your task is to find the total number of characters to represent the newly encoded strings minus the number of characters of code in each original string literal. For example, for the strings above, the total encoded length (6 + 9 + 16 + 11 = 42) minus the characters in the original code representation (23, just like in the first part of this puzzle) is 42 - 23 = 19.
%   
%   Your puzzle answer was 2085.
%   
%   Both parts of this puzzle are complete! They provide two gold stars: **
%   
%   At this point, all that is left is for you to admire your advent calendar.
%   
%   If you still want to see it, you can get your puzzle input.
%   
%   You can also [Share] this puzzle.
%   
