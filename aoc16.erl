-module(aoc16).
-export([start/0]).

start() ->
  L = get_data_list(),
  lists:foldl(
    fun({Num,Plis}, Acc) ->
      Result =
      lists:all(
        fun
          ({<<"children">>,<<"3">>}) -> true;
          %({<<"cats">>,<<"7">>}) -> true;
          ({<<"cats">>,N}) ->
            binary_to_integer(N) > 7;
          ({<<"samoyeds">>,<<"2">>}) -> true;
          %({<<"pomeranians">>,<<"3">>}) -> true;
          ({<<"pomeranians">>,N}) ->
            binary_to_integer(N) < 3;
          ({<<"akitas">>,<<"0">>}) -> true;
          ({<<"vizslas">>,<<"0">>}) -> true;
          %({<<"goldfish">>,<<"5">>}) -> true;
          ({<<"goldfish">>,N}) ->
            binary_to_integer(N) < 5;
          %({<<"trees">>,<<"3">>}) -> true;
          ({<<"trees">>,N}) ->
            binary_to_integer(N) > 3;
          ({<<"cars">>,<<"2">>}) -> true;
          ({<<"perfumes">>,<<"1">>}) -> true;
          ({_,_}) -> false
        end, Plis),
      case Result of
        true -> [{Num,Plis}|Acc];
        false -> Acc
      end
    end, [], L).
     
get_data_list() ->
  {ok, Bin} = file:read_file("aoc16.txt"),
  [begin
    [_,Num,K0,V0,K1,V1,K2,V2] = 
      binary:split(B, [<<" ">>, <<", ">>, <<": ">>], [global]),
    {Num, [{K0,V0},{K1,V1},{K2,V2}]}
   end || B <- binary:split(Bin, [<<"\n">>], [global]), size(B) > 0].



%   --- Day 16: Aunt Sue ---
%   
%   Your Aunt Sue has given you a wonderful gift, and you'd like to send her a thank you card. However, there's a small problem: she signed it "From, Aunt Sue".
%   
%   You have 500 Aunts named "Sue".
%   
%   So, to avoid sending the card to the wrong person, you need to figure out which Aunt Sue (which you conveniently number 1 to 500, for sanity) gave you the gift. You open the present and, as luck would have it, good ol' Aunt Sue got you a My First Crime Scene Analysis Machine! Just what you wanted. Or needed, as the case may be.
%   
%   The My First Crime Scene Analysis Machine (MFCSAM for short) can detect a few specific compounds in a given sample, as well as how many distinct kinds of those compounds there are. According to the instructions, these are what the MFCSAM can detect:
%   
%   children, by human DNA age analysis.
%   cats. It doesn't differentiate individual breeds.
%   Several seemingly random breeds of dog: samoyeds, pomeranians, akitas, and vizslas.
%   goldfish. No other kinds of fish.
%   trees, all in one group.
%   cars, presumably by exhaust or gasoline or something.
%   perfumes, which is handy, since many of your Aunts Sue wear a few kinds.
%   In fact, many of your Aunts Sue have many of these. You put the wrapping from the gift into the MFCSAM. It beeps inquisitively at you a few times and then prints out a message on ticker tape:
%   
%   children: 3
%   cats: 7
%   samoyeds: 2
%   pomeranians: 3
%   akitas: 0
%   vizslas: 0
%   goldfish: 5
%   trees: 3
%   cars: 2
%   perfumes: 1
%   You make a list of the things you can remember about each Aunt Sue. Things missing from your list aren't zero - you simply don't remember the value.
%   
%   What is the number of the Sue that got you the gift?
%   
%   Your puzzle answer was 213.
%   
%   --- Part Two ---
%   
%   As you're about to send the thank you note, something in the MFCSAM's instructions catches your eye. Apparently, it has an outdated retroencabulator, and so the output from the machine isn't exact values - some of them indicate ranges.
%   
%   In particular, the cats and trees readings indicates that there are greater than that many (due to the unpredictable nuclear decay of cat dander and tree pollen), while the pomeranians and goldfish readings indicate that there are fewer than that many (due to the modial interaction of magnetoreluctance).
%   
%   What is the number of the real Aunt Sue?
%   
%   Your puzzle answer was 323.
%   
%   Both parts of this puzzle are complete! They provide two gold stars: **
%   
%   At this point, all that is left is for you to admire your advent calendar.
%   
%   If you still want to see it, you can get your puzzle input.
%   
%   You can also [Share] this puzzle.
%   
