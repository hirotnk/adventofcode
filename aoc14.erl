-module(aoc14).
-export([start/0]).

start() ->
  ets:new(config, [set, named_table]),
  ets:new(race, [set, named_table]),
  get_data_list(),
  io:format("~p~n", [ets:tab2list(config)]),
  L = [RDeer || {RDeer, _} <- ets:tab2list(config)],
  Result = lists:max(race(2503, L)),
  ets:delete(config),
  ets:delete(race),
  Result.

race(End, L) ->
  lists:foldl(
    fun(RDeer, Acc) ->
      {Speed, Runnable, Sleep} = ets:lookup_element(config, RDeer, 2),
      Where = where_is_rd(End, Runnable, Sleep, running, {Speed, Runnable, Sleep}, 0),
      io:format("~p~p~n", [RDeer, Where]),
      [{Where, RDeer}|Acc]
    end, [], L).
    

where_is_rd(0, _, _, _, {Speed, Duration, Rest}, Where) -> Where;
where_is_rd(End, 0, Sleep, running, {Speed, Duration, Rest}, Where) ->
  % running -> sleeping
  where_is_rd(End-1, 0, Rest-1, sleeping, {Speed, Duration, Rest}, Where);

where_is_rd(End, Runnable, Sleep, running, {Speed, Duration, Rest}, Where) ->
  % running -> running
  where_is_rd(End-1, Runnable-1, Sleep, running, {Speed, Duration, Rest}, Where+Speed);

where_is_rd(End, _Duration, 0, sleeping, {Speed, Duration, Rest}, Where) ->
  % sleeping -> running
  where_is_rd(End-1, Duration-1, Rest, running, {Speed, Duration, Rest}, Where+Speed);

where_is_rd(End, Runnable, Sleep, sleeping, {Speed, Duration, Rest}, Where) ->
  % sleeping -> sleeping
  where_is_rd(End-1, Runnable, Sleep-1, sleeping, {Speed, Duration, Rest}, Where).

  
get_data_list() ->
  {ok, Bin} = file:read_file("aoc14.txt"),
  [case binary:split(Line, [<<" ">>], [global]) of
     [A,_,_, P, _, _, Durable, _, _, _, _, _, _, B, _] ->
       ets:insert(config, {A,{binary_to_integer(P),binary_to_integer(Durable),binary_to_integer(B)}}), A
   end || Line <- binary:split(Bin, [<<".\n">>], [global]), size(Line) > 1].



%   --- Day 14: Reindeer Olympics ---
%   
%   This year is the Reindeer Olympics! Reindeer can fly at high speeds, but must rest occasionally to recover their energy. Santa would like to know which of his reindeer is fastest, and so he has them race.
%   
%   Reindeer can only either be flying (always at their top speed) or resting (not moving at all), and always spend whole seconds in either state.
%   
%   For example, suppose you have the following Reindeer:
%   
%   Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.
%   Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds.
%   After one second, Comet has gone 14 km, while Dancer has gone 16 km. After ten seconds, Comet has gone 140 km, while Dancer has gone 160 km. On the eleventh second, Comet begins resting (staying at 140 km), and Dancer continues on for a total distance of 176 km. On the 12th second, both reindeer are resting. They continue to rest until the 138th second, when Comet flies for another ten seconds. On the 174th second, Dancer flies for another 11 seconds.
%   
%   In this example, after the 1000th second, both reindeer are resting, and Comet is in the lead at 1120 km (poor Dancer has only gotten 1056 km by that point). So, in this situation, Comet would win (if the race ended at 1000 seconds).
%   
%   Given the descriptions of each reindeer (in your puzzle input), after exactly 2503 seconds, what distance has the winning reindeer traveled?
%   
%   Your puzzle answer was 2660.
%   
%   --- Part Two ---
%   
%   Seeing how reindeer move in bursts, Santa decides he's not pleased with the old scoring system.
%   
%   Instead, at the end of each second, he awards one point to the reindeer currently in the lead. (If there are multiple reindeer tied for the lead, they each get one point.) He keeps the traditional 2503 second time limit, of course, as doing otherwise would be entirely ridiculous.
%   
%   Given the example reindeer from above, after the first second, Dancer is in the lead and gets one point. He stays in the lead until several seconds into Comet's second burst: after the 140th second, Comet pulls into the lead and gets his first point. Of course, since Dancer had been in the lead for the 139 seconds before that, he has accumulated 139 points by the 140th second.
%   
%   After the 1000th second, Dancer has accumulated 689 points, while poor Comet, our old champion, only has 312. So, with the new scoring system, Dancer would win (if the race ended at 1000 seconds).
%   
%   Again given the descriptions of each reindeer (in your puzzle input), after exactly 2503 seconds, how many points does the winning reindeer have?
%   
%   Your puzzle answer was 1256.
%   
%   Both parts of this puzzle are complete! They provide two gold stars: **
%   
%   At this point, all that is left is for you to admire your advent calendar.
%   
%   If you still want to see it, you can get your puzzle input.
%   
%   You can also [Share] this puzzle.
%   
