-module(aoc14_part2).
-export([start/0]).

start() ->
  ets:new(config, [set, named_table]),
  ets:new(race, [set, named_table]),
  ets:new(points, [set, named_table]),
  get_data_list(),
  L = [RDeerConfig || RDeerConfig <- ets:tab2list(config)],
  lists:foreach(fun({RDeer, _}) -> ets:insert(race, {RDeer,0}) end, L),
  Result = race_ite(2503, L),
  ets:delete(config),
  ets:delete(race),
  ets:delete(points),
  Result.

race_ite(0, L) -> 
  lists:max([Points || {RDeer, Points} <- ets:tab2list(points)]) ;
race_ite(Now, L) -> 
  run_race(Now, L),
  race_ite(Now-1, L).

run_race(End0, L) ->
  lists:map(
    fun({RDeer, {Speed, Runnable, Sleep}}) ->
      Result =
          case ets:lookup_element(race, RDeer, 2) of
            0 -> 
              where_is_rd(End0, Runnable, Sleep, running, {Speed, Runnable, Sleep}, 0);
            {Where, Runnable0, Sleep0, State, {Speed, Duration, Rest}} ->
              where_is_rd(End0, Runnable0, Sleep0, State, {Speed, Runnable, Sleep}, Where)
          end,
      ets:insert(race, {RDeer, Result})
    end, L),
  reward().

reward() ->
  {MaxScore, _, _, _, _} = lists:max([Current || {RDeer, Current} <- ets:tab2list(race)]),
  ets:foldl(
    fun({RDeer, {Score, A, B, C, D}},Acc) ->
      case Score == MaxScore of
        true -> ets:update_counter(points, RDeer, {2,1});
        false -> ok
      end, Acc
    end, dummy, race).

where_is_rd(0, _, _, _, {Speed, Duration, Rest}, Where) ->
  {Where, 0, Rest-1, sleeping, {Speed, Duration, Rest}};
where_is_rd(_Now, 0, Sleep, running, {Speed, Duration, Rest}, Where) ->
  % running -> sleeping
  {Where, 0, Rest-1, sleeping, {Speed, Duration, Rest}};
where_is_rd(_Now, Runnable, Sleep, running, {Speed, Duration, Rest}, Where) ->
  % running -> running
  {Where+Speed, Runnable-1, Sleep, running, {Speed, Duration, Rest}};
where_is_rd(_Now, _Duration, 0, sleeping, {Speed, Duration, Rest}, Where) ->
  % sleeping -> running
  {Where+Speed, Duration-1, Rest, running, {Speed, Duration, Rest}};
where_is_rd(_Now, Runnable, Sleep, sleeping, {Speed, Duration, Rest}, Where) ->
  % sleeping -> sleeping
  {Where,  Runnable, Sleep-1, sleeping, {Speed, Duration, Rest}}.

get_data_list() ->
  {ok, Bin} = file:read_file("aoc14.txt"),
  [case binary:split(Line, [<<" ">>], [global]) of
     [A,_,_, P, _, _, Durable, _, _, _, _, _, _, B, _] ->
       ets:insert(config, {A,{binary_to_integer(P),binary_to_integer(Durable),binary_to_integer(B)}}),
       ets:insert(points, {A,0}), A
   end || Line <- binary:split(Bin, [<<".\n">>], [global]), size(Line) > 1].

