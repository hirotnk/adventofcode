-module(aoc09).
-export([start/0]).

start() ->
  ets:new(distmap, [set, named_table]),
  ets:new(placelist, [set, named_table]),
  get_data_list(),
  AllRoutes = make_all_route([P || [{P,P}] <- ets:match(placelist, '$0')]),
  % Part 1
  %[{Cost,Route} | _] = lists:sort(process_all_routes(AllRoutes)),
  
  % Part 2
  {Cost,Route} =
    lists:foldl(
      fun
        ({infinity,_},CR) -> CR
        ;({C,R},{C0,R0}) when C > C0 -> {C,R}
        ;(_, CR) -> CR
      end, {0, dummy_route}, lists:sort(process_all_routes(AllRoutes))),
  ets:delete(distmap),
  ets:delete(placelist),
  {Cost, Route}.

process_all_routes([]) -> [];
process_all_routes([Route|Routes]) ->
  [{process_route(Route, 0), Route} | process_all_routes(Routes)].

process_route([], Cost) -> Cost;
process_route([_], Cost) -> Cost;
process_route([P0,P1|T], Cost) ->
  case catch ets:lookup_element(distmap, {P0, P1}, 2) of
    {'EXIT', _} -> infinity;
    C -> process_route([P1|T], Cost+C)
  end.

get_data_list() ->
  {ok, Bin} = file:read_file("aoc09.txt"),
  [begin
    [P0,_,P1,_,Dist] = string:tokens(binary_to_list(B), " "),
    io:format("~p~n", [B]),
    ets:insert(placelist, {P0,P0}),
    ets:insert(placelist, {P1,P1}),
    ets:insert(distmap, {{P0,P1},list_to_integer(Dist)}),
    ets:insert(distmap, {{P1,P0},list_to_integer(Dist)})
   end || B <- binary:split(Bin, [<<"\n">>], [global]), size(B) > 0].

make_all_route([]) -> [[]];
make_all_route(L) ->
  lists:flatmap(
    fun(E) ->
      [[E | SubList] || SubList <- make_all_route(L -- [E])]
    end, L).


%   --- Day 9: All in a Single Night ---
%   
%   Every year, Santa manages to deliver all of his presents in a single night.
%   
%   This year, however, he has some new locations to visit; his elves have provided him the distances between every pair of locations. He can start and end at any two (different) locations he wants, but he must visit each location exactly once. What is the shortest distance he can travel to achieve this?
%   
%   For example, given the following distances:
%   
%   London to Dublin = 464
%   London to Belfast = 518
%   Dublin to Belfast = 141
%   The possible routes are therefore:
%   
%   Dublin -> London -> Belfast = 982
%   London -> Dublin -> Belfast = 605
%   London -> Belfast -> Dublin = 659
%   Dublin -> Belfast -> London = 659
%   Belfast -> Dublin -> London = 605
%   Belfast -> London -> Dublin = 982
%   The shortest of these is London -> Dublin -> Belfast = 605, and so the answer is 605 in this example.
%   
%   What is the distance of the shortest route?
%   
%   Your puzzle answer was 207.
%   
%   --- Part Two ---
%   
%   The next year, just to show off, Santa decides to take the route with the longest distance instead.
%   
%   He can still start and end at any two (different) locations he wants, and he still must visit each location exactly once.
%   
%   For example, given the distances above, the longest route would be 982 via (for example) Dublin -> London -> Belfast.
%   
%   What is the distance of the longest route?
%   
%   Your puzzle answer was 804.
%   
%   Both parts of this puzzle are complete! They provide two gold stars: **
%   
%   At this point, all that is left is for you to admire your advent calendar.
%   
%   If you still want to see it, you can get your puzzle input.
%   
%   
