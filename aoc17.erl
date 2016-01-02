-module(aoc17).
-export([start/0]).


start() ->
  min_container(150, [33,14,18,20,45,35,16,35,1,13,18,13,50,44,48,6,24,41,30,42]).

min_container(Target, Containers) ->
  L = ways(Target, Containers, []),
  lists:foldl(
    fun(E,{Min,Cnt}) ->
      if
        length(E) < Min -> {length(E), 1};
        length(E) == Min -> {Min, Cnt+1};
        true -> {Min,Cnt}
      end
    end, {length(Containers), 0}, L).


ways(Target, [], Acc) -> [];
ways(Target, [H|T], Acc) ->
  Rest = Target - H,
  if
    Rest == 0 -> io:format("~p~n", [[H|Acc]]), [[H|Acc]] ++ ways(Target, T, Acc);
    Rest < 0  -> ways(Target, T, Acc);
    true      -> ways(Rest, T, [H|Acc]) ++ ways(Target, T, Acc)
  end.


%   --- Day 17: No Such Thing as Too Much ---
%   
%   The elves bought too much eggnog again - 150 liters this time. To fit it all into your refrigerator, you'll need to move it into smaller containers. You take an inventory of the capacities of the available containers.
%   
%   For example, suppose you have containers of size 20, 15, 10, 5, and 5 liters. If you need to store 25 liters, there are four ways to do it:
%   
%   15 and 10
%   20 and 5 (the first 5)
%   20 and 5 (the second 5)
%   15, 5, and 5
%   Filling all containers entirely, how many different combinations of containers can exactly fit all 150 liters of eggnog?
%   
%   Your puzzle answer was 1304.
%   
%   --- Part Two ---
%   
%   While playing with all the containers in the kitchen, another load of eggnog arrives! The shipping and receiving department is requesting as many containers as you can spare.
%   
%   Find the minimum number of containers that can exactly fit all 150 liters of eggnog. How many different ways can you fill that number of containers and still hold exactly 150 litres?
%   
%   In the example above, the minimum number of containers was two. There were three ways to use that many containers, and so the answer there would be 3.
%   
%   Your puzzle answer was 18.
%   
%   Both parts of this puzzle are complete! They provide two gold stars: **
%   
%   At this point, all that is left is for you to admire your advent calendar.
%   
%   If you still want to see it, you can get your puzzle input.
%   
%   You can also [Share] this puzzle.
%   
