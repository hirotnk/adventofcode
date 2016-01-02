-module(aoc18).
-compile(export_all).
-export([start/0]).

start() ->
  ets:new(grid, [set, named_table]),
  init(get_data_list(), 0),
  CheckNext = fun(X,Y) -> case is_next_on(X,Y) of true -> 1; false -> 0 end end,
  F = 
    fun
     Check(100,_) -> []
    ;Check(X,100) ->
       X0 = X+1, Y0 = 0,
       Val = CheckNext(X0,Y0),
       [{{X0,Y0}, Val}|Check(X0, Y0+1)]
    ;Check(X0,Y0) ->
       Val = CheckNext(X0,Y0),
       [{{X0,Y0}, Val}|Check(X0, Y0+1)]
    end,
  lists:foreach(
    fun(_) ->
      NextVals = F(0,0),
      lists:foreach(fun(E) -> ets:insert(grid, E) end, NextVals)
    end,
    lists:seq(1, 100)),
  Result =
    ets:foldl(
      fun
        ({_, 1}, Cnt) -> Cnt + 1
       ;({_, 0}, Cnt) -> Cnt
      end, 0, grid),
  ets:delete(grid),
  Result.

% part 2
%is_next_on(X,Y) when
%   (X == 0 andalso (Y == 0 orelse Y == 99)) orelse
%   (X == 99 andalso (Y == 0 orelse Y == 99)) -> true;
is_next_on(X,Y) ->
  Val  = get_state(X,Y),
  State =
  [get_state(X-1,Y-1), get_state(X-1,Y), get_state(X-1,Y+1),
   get_state(X,Y+1), get_state(X,Y-1), get_state(X+1,Y-1),
   get_state(X+1,Y), get_state(X+1,Y+1)],
  case Val of
    1 -> lists:sum(State) == 2 orelse lists:sum(State) == 3;
    0 -> lists:sum(State) == 3
  end.

get_state(X,Y) ->
  case catch ets:lookup_element(grid, {X,Y}, 2) of
    {'EXIT', _} -> 0;
    V -> V
  end.

update(0,0,100, 100) ->
  F = 
    fun
     Check(100,100,100,100) -> ok;
     Check(X0,Y0,X1,Y1) ->
      Val = ets:lookup_element(grid, {X0,Y0}, 2),
      io:format("~p=~p~n", [{X0,Y0}, Val]),
      Check(X0+1, Y0+1, X1, Y1)
    end,
  F(0,0,100, 100).

get_data_list() ->
  {ok, Bin} = file:read_file("aoc18.txt"),
  [binary_to_list(B) || B <- binary:split(Bin, [<<"\n">>], [global]), size(B) > 0].

init([], N) -> N;
init([B|Rest], N) ->
  lists:foldl(
    fun(E,Cnt) when Cnt =< 99 ->
      ets:insert(grid, {{N, Cnt}, case E of $# -> 1; $. -> 0 end}),
      Cnt+1
    end, 0, B),
  init(Rest, N+1).




%   --- Day 18: Like a GIF For Your Yard ---
%   
%   After the million lights incident, the fire code has gotten stricter: now, at
%   most ten thousand lights are allowed. You arrange them in a 100x100 grid.
%   
%   Never one to let you down, Santa again mails you instructions on the ideal
%   lighting configuration. With so few lights, he says, you'll have to resort to
%   animation.
%   
%   Start by setting your lights to the included initial configuration (your puzzle
%   input). A # means "on", and a . means "off".
%   
%   Then, animate your grid in steps, where each step decides the next
%   configuration based on the current one. Each light's next state (either on or
%   off) depends on its current state and the current states of the eight lights
%   adjacent to it (including diagonals). Lights on the edge of the grid might have
%   fewer than eight neighbors; the missing ones always count as "off".
%   
%   For example, in a simplified 6x6 grid, the light marked A has the neighbors
%   numbered 1 through 8, and the light marked B, which is on an edge, only has the
%   neighbors marked 1 through 5:
%   
%   1B5...  234...  ......  ..123.  ..8A4.  ..765.  The state a light should have
%   next is based on its current state (on or off) plus the number of neighbors
%   that are on:
%   
%   A light which is on stays on when 2 or 3 neighbors are on, and turns off
%   otherwise.  A light which is off turns on if exactly 3 neighbors are on, and
%   stays off otherwise.  All of the lights update simultaneously; they all
%   consider the same current state before moving to the next.
%   
%   Here's a few steps from an example configuration of another 6x6 grid:
%   
%   Initial state: .#.#.# ...##.  #....# ..#...  #.#..# ####..
%   
%   After 1 step: ..##..  ..##.# ...##.  ......  #.....  #.##..
%   
%   After 2 steps: ..###.  ......  ..###.  ......  .#....  .#....
%   
%   After 3 steps: ...#..  ......  ...#..  ..##..  ......  ......
%   
%   After 4 steps: ......  ......  ..##..  ..##..  ......  ......  After 4 steps,
%   this example has four lights on.
%   
%   In your grid of 100x100 lights, given your initial configuration, how many
%   lights are on after 100 steps?
%   
%   Your puzzle answer was 768.
%   
%   --- Part Two ---
%   
%   You flip the instructions over; Santa goes on to point out that this is all
%   just an implementation of Conway's Game of Life. At least, it was, until you
%   notice that something's wrong with the grid of lights you bought: four lights,
%   one in each corner, are stuck on and can't be turned off. The example above
%   will actually run like this:
%   
%   Initial state: ##.#.# ...##.  #....# ..#...  #.#..# ####.#
%   
%   After 1 step: #.##.# ####.# ...##.  ......  #...#.  #.####
%   
%   After 2 steps: #..#.# #....# .#.##.  ...##.  .#..## ##.###
%   
%   After 3 steps: #...## ####.# ..##.# ......  ##....  ####.#
%   
%   After 4 steps: #.#### #....# ...#..  .##...  #.....  #.#..#
%   
%   After 5 steps: ##.### .##..# .##...  .##...  #.#...  ##...# After 5 steps, this
%   example now has 17 lights on.
%   
%   In your grid of 100x100 lights, given your initial configuration, but with the
%   four corners always in the on state, how many lights are on after 100 steps?
%   
%   Your puzzle answer was 781.
%   
%   Both parts of this puzzle are complete! They provide two gold stars: **
%   
%   At this point, all that is left is for you to admire your advent calendar.
%   
%   If you still want to see it, you can get your puzzle input.
%   
%   You can also [Share] this puzzle.
%   
