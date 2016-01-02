-module(aoc03).
-export([start/0]).

start() ->
  L = get_data_list(),
  ThisYear = count_houses(L, {0, 0}, sets:add_element({0, 0}, sets:new())),
  NextYear = count_houses_with_robo(L, 0, {{0,0}, sets:add_element({0,0}, sets:new())}, {{0,0},sets:new()}),
  io:format("ThisYear:~p NextYear:~p~n", [ThisYear, NextYear]).

get_data_list() ->
  {ok, Bin} = file:read_file("aoc03.txt"),
  binary_to_list(Bin).

count_houses_with_robo([], _Turn, {_, SantaHouses}, {_, RoboHouses}) ->
  sets:size(sets:union(SantaHouses, RoboHouses));
count_houses_with_robo([H|T], Turn, P0 = {{X0,Y0}, SantaHouses}, P1 = {{X1, Y1}, RoboHouses}) ->
  case Turn of
    0 ->
      House = get_point(H, X0, Y0),
      count_houses_with_robo(T, 1, {House, sets:add_element(House, SantaHouses)}, P1);
    1 ->
      House = get_point(H, X1, Y1),
      count_houses_with_robo(T, 0, P0, {House, sets:add_element(House, RoboHouses)})
  end.

count_houses([], _, Set) -> io:format("~p~n", [Set]), sets:size(Set);
count_houses([H|T], {X, Y}, Set) -> 
  HouseVisited = get_point(H, X, Y),
  case sets:is_element(HouseVisited, Set) of
    true -> ok; %io:format("already visited:~p~n", [HouseVisited]);
    false -> ok
  end,
  count_houses(T, HouseVisited, sets:add_element(HouseVisited, Set)).

get_point(H, X, Y) ->
  case H of
    $^ -> {X,Y+1};
    $> -> {X+1,Y};
    $v -> {X,Y-1};
    $< -> {X-1,Y};
    Err ->
      %io:format("Unknown:~p , Skipping~n", [Err]),
      {0, 0}
  end.


%--- Day 3: Perfectly Spherical Houses in a Vacuum ---
%
%Santa is delivering presents to an infinite two-dimensional grid of houses.
%
%He begins by delivering a present to the house at his starting location, and then an elf at the North Pole calls him via radio and tells him where to move next. Moves are always exactly one house to the north (^), south (v), east (>), or west (<). After each move, he delivers another present to the house at his new location.
%
%However, the elf back at the north pole has had a little too much eggnog, and so his directions are a little off, and Santa ends up visiting some houses more than once. How many houses receive at least one present?
%
%For example:
%
%> delivers presents to 2 houses: one at the starting location, and one to the east.
%^>v< delivers presents to 4 houses in a square, including twice to the house at his starting/ending location.
%^v^v^v^v^v delivers a bunch of presents to some very lucky children at only 2 houses.
%Your puzzle answer was 2572.
%
%--- Part Two ---
%
%The next year, to speed up the process, Santa creates a robot version of himself, Robo-Santa, to deliver presents with him.
%
%Santa and Robo-Santa start at the same location (delivering two presents to the same starting house), then take turns moving based on instructions from the elf, who is eggnoggedly reading from the same script as the previous year.
%
%This year, how many houses receive at least one present?
%
%For example:
%
%^v delivers presents to 3 houses, because Santa goes north, and then Robo-Santa goes south.
%^>v< now delivers presents to 3 houses, and Santa and Robo-Santa end up back where they started.
%^v^v^v^v^v now delivers presents to 11 houses, with Santa going one direction and Robo-Santa going the other.
%Your puzzle answer was 2631.
%
%Both parts of this puzzle are complete! They provide two gold stars: **
%
%At this point, all that is left is for you to admire your advent calendar.
%
%If you still want to see it, you can get your puzzle input.
%
%You can also [Share] this puzzle.
%
