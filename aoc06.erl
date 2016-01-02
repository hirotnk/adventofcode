-module(aoc06).
-compile(export_all).

start() ->
  ets:new(lights, [set, named_table]),
  change({0,0},{999,999},off),
  get_data_list(),
  Cnt = ets:foldl( fun ({_, Val}, Cnt) -> Cnt + Val end, 0, lights),
  io:format("~p lights are on~n", [Cnt]),
  ets:delete(lights).

get_data_list() ->
  {ok, Bin} = file:read_file("aoc06.txt"),
  [case  binary:split(B, [<<" ">>], [global]) of
     [<<"turn">>,<<"on">>, Num0, _, Num1] -> change(to_point(Num0), to_point(Num1), on);
     [<<"turn">>,<<"off">>, Num0, _, Num1] -> change(to_point(Num0), to_point(Num1), off);
     [<<"toggle">>, Num0, _, Num1] -> change(to_point(Num0), to_point(Num1), toggle)
   end || B <- binary:split(Bin, [<<"\n">>], [global]), size(B) > 0].

to_point(Num) ->
  [X,Y] = binary:split(Num, [<<",">>], [global]),
  {binary_to_integer(X),binary_to_integer(Y)}.

change({X0,Y0},{X1,Y1}, Command) ->
  F =
    fun Dloop(X,Y) ->
      case {X,Y} of
        {X1,Y1} -> command2(X1,Y1,Command), ok;
        {X1,YB} -> command2(X1,YB,Command), Dloop(X1,YB+1);
        {XA,Y1} -> command2(XA,Y1,Command), Dloop(XA+1,Y0);
        {XA,YA} -> command2(XA,YA,Command), Dloop(XA,YA+1)
      end
    end,
  F(X0,Y0).
        
command(X,Y,Command) ->
  case Command of
    toggle ->
      case ets:lookup_element(lights, {X,Y}, 2) of
        on -> ets:insert(lights, {{X,Y}, off});
        off -> ets:insert(lights, {{X,Y}, on})
      end;
    Command -> ets:insert(lights, {{X,Y}, Command})
  end.

command2(X,Y,Command) ->
  case catch ets:lookup_element(lights, {X,Y}, 2) of
    {'EXIT', _} -> ets:insert(lights, {{X,Y}, 0});
    Val ->
      case Command of
        toggle -> ets:insert(lights, {{X,Y}, Val+2});
        on -> ets:insert(lights, {{X,Y}, Val+1});
        off ->
          Val1 =
            if
               Val-1 =< 0 -> 0;
               true -> Val-1
            end,
          ets:insert(lights, {{X,Y}, Val1})
      end
  end.


%   --- Day 6: Probably a Fire Hazard ---
%   
%   Because your neighbors keep defeating you in the holiday house decorating contest year after year, you've decided to deploy one million lights in a 1000x1000 grid.
%   
%   Furthermore, because you've been especially nice this year, Santa has mailed you instructions on how to display the ideal lighting configuration.
%   
%   Lights in your grid are numbered from 0 to 999 in each direction; the lights at each corner are at 0,0, 0,999, 999,999, and 999,0. The instructions include whether to turn on, turn off, or toggle various inclusive ranges given as coordinate pairs. Each coordinate pair represents opposite corners of a rectangle, inclusive; a coordinate pair like 0,0 through 2,2 therefore refers to 9 lights in a 3x3 square. The lights all start turned off.
%   
%   To defeat your neighbors this year, all you have to do is set up your lights by doing the instructions Santa sent you in order.
%   
%   For example:
%   
%   turn on 0,0 through 999,999 would turn on (or leave on) every light.
%   toggle 0,0 through 999,0 would toggle the first line of 1000 lights, turning off the ones that were on, and turning on the ones that were off.
%   turn off 499,499 through 500,500 would turn off (or leave off) the middle four lights.
%   After following the instructions, how many lights are lit?
%   
%   Your puzzle answer was 400410.
%   
%   --- Part Two ---
%   
%   You just finish implementing your winning light pattern when you realize you mistranslated Santa's message from Ancient Nordic Elvish.
%   
%   The light grid you bought actually has individual brightness controls; each light can have a brightness of zero or more. The lights all start at zero.
%   
%   The phrase turn on actually means that you should increase the brightness of those lights by 1.
%   
%   The phrase turn off actually means that you should decrease the brightness of those lights by 1, to a minimum of zero.
%   
%   The phrase toggle actually means that you should increase the brightness of those lights by 2.
%   
%   What is the total brightness of all lights combined after following Santa's instructions?
%   
%   For example:
%   
%   turn on 0,0 through 0,0 would increase the total brightness by 1.
%   toggle 0,0 through 999,999 would increase the total brightness by 2000000.
%   Your puzzle answer was 15343601.
%   
%   Both parts of this puzzle are complete! They provide two gold stars: **
%   
%   At this point, all that is left is for you to admire your advent calendar.
%   
%   If you still want to see it, you can get your puzzle input.
%   
%   You can also [Share] this puzzle.
%   
