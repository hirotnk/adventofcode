-module(aoc10).
-export([start/0]).

start() ->
  length(process_n_times("1113222113", 0, 50)).

process_n_times(Seq, Cnt, Max) when Cnt >= Max -> Seq;
process_n_times(Seq, Cnt, Max) ->
  Next = process_one_seq(Seq, 0, none),
  process_n_times(Next, Cnt+1, Max). 

process_one_seq([],Cnt,S) -> integer_to_list(Cnt) ++ [S];
process_one_seq([H|T],0,none) -> process_one_seq(T,1,H);
process_one_seq([S|T],Cnt,S) -> process_one_seq(T,Cnt+1,S);
process_one_seq([H|T],Cnt,S) -> integer_to_list(Cnt) ++ [S] ++ process_one_seq(T,1,H).

    
%--- Day 10: Elves Look, Elves Say ---
%
%Today, the Elves are playing a game called look-and-say. They take turns making sequences by reading aloud the previous sequence and using that reading as the next sequence. For example, 211 is read as "one two, two ones", which becomes 1221 (1 2, 2 1s).
%
%Look-and-say sequences are generated iteratively, using the previous value as input for the next step. For each step, take the previous value, and replace each run of digits (like 111) with the number of digits (3) followed by the digit itself (1).
%
%For example:
%
%1 becomes 11 (1 copy of digit 1).
%11 becomes 21 (2 copies of digit 1).
%21 becomes 1211 (one 2 followed by one 1).
%1211 becomes 111221 (one 1, one 2, and two 1s).
%111221 becomes 312211 (three 1s, two 2s, and one 1).
%Starting with the digits in your puzzle input, apply this process 40 times. What is the length of the result?
%
%Your puzzle answer was 252594.
%
%--- Part Two ---
%
%Neat, right? You might also enjoy hearing John Conway talking about this sequence (that's Conway of Conway's Game of Life fame).
%
%Now, starting again with the digits in your puzzle input, apply this process 50 times. What is the length of the new result?
%
%Your puzzle answer was 3579328.
%
%Both parts of this puzzle are complete! They provide two gold stars: **
%
%At this point, all that is left is for you to admire your advent calendar.
%
%Your puzzle input was 1113222113.
%
%
