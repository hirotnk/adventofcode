-module(aoc4).
-export([start/0]).

start() ->
  get_pass(1).

get_pass(N) ->
  case lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B>> <= erlang:md5("bgvyzdsv" ++ integer_to_list(N))]) of
    "000000" ++ Rest -> N;
    _ -> get_pass(N+1)
  end.

%--- Day 4: The Ideal Stocking Stuffer ---
%
%Santa needs help mining some AdventCoins (very similar to bitcoins) to use as gifts for all the economically forward-thinking little girls and boys.
%
%To do this, he needs to find MD5 hashes which, in hexadecimal, start with at least five zeroes. The input to the MD5 hash is some secret key (your puzzle input, given below) followed by a number in decimal. To mine AdventCoins, you must find Santa the lowest positive number (no leading zeroes: 1, 2, 3, ...) that produces such a hash.
%
%For example:
%
%If your secret key is abcdef, the answer is 609043, because the MD5 hash of abcdef609043 starts with five zeroes (000001dbbfa...), and it is the lowest such number to do so.
%If your secret key is pqrstuv, the lowest number it combines with to make an MD5 hash starting with five zeroes is 1048970; that is, the MD5 hash of pqrstuv1048970 looks like 000006136ef....
%Your puzzle answer was 254575.
%
%--- Part Two ---
%
%Now find one that starts with six zeroes.
%
%Your puzzle answer was 1038736.
%
%Both parts of this puzzle are complete! They provide two gold stars: **
%
%At this point, all that is left is for you to admire your advent calendar.
%
%Your puzzle input was bgvyzdsv.
%
