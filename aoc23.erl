-module(aoc23).
-compile(export_all).
-export([start/0]).

start() ->
  ets:new(program, [set, named_table]),
  L0 = get_data_list(),
  lists:foreach(
    fun(Line) -> ets:insert(program, Line) end,
    lists:zip(lists:seq(1,length(L0)), L0)),
  io:format("~p~n", [lists:sort(ets:match (program, '$0'))]),
  A = execute(1, 1, 0),
  ets:delete(program),
  A.

get_data_list() ->
  {ok, Bin} = file:read_file("aoc23.txt"),
  [case binary:split(B, [<<", ">>, <<" ">>], [global]) of
     [Inst, Register, Offset] -> {Inst, Register, binary_to_integer(Offset)};
     [<<"jmp">>, Offset]      -> {<<"jmp">>, binary_to_integer(Offset)};
     [Inst, Register]         -> {Inst, Register}
   end || B <- binary:split(Bin, [<<"\n">>], [global]), size(B) > 0].

execute(Num, A, B) ->
  case catch ets:lookup_element(program, Num, 2) of
    {'EXIT', _} -> {A, B};
    {Inst, R} when Inst == <<"hlf">> orelse
                   Inst == <<"inc">> orelse
                   Inst == <<"tpl">> ->
      {A1, B1} = register_operate(Inst, R, A, B),
      execute(Num + 1, A1, B1);
    {<<"jmp">>, Offset} -> execute(Num + Offset, A, B);
    {Inst, R, Offset} ->
      execute(Num + goto_operate(Inst, R, A, B, Offset), A, B)
  end.

goto_operate(Inst, R, A, B, Offset) ->
  io:format("Inst:~p R:~p A:~p B:~p Offset:~p ~n", [Inst, R, A, B, Offset]),
  RR =
  case R of
    <<"a">> -> A;
    <<"b">> -> B
  end,
  case Inst of
    <<"jio">> ->
      case RR of
        1 -> Offset;
        _ -> 1
      end;
    <<"jie">> ->
      case RR rem 2 of
        0 -> Offset;
        1 -> 1
      end
  end.

register_operate(<<"hlf">>, R, A, B) ->
  case R of
    <<"a">> -> {A bsr 1, B};
    <<"b">> -> {A, B bsr 1}
  end;
register_operate(<<"inc">>, R, A, B) ->
  case R of
    <<"a">> -> {A + 1, B};
    <<"b">> -> {A, B + 1}
  end;
register_operate(<<"tpl">>, R, A, B) ->
  case R of
    <<"a">> -> {A * 3, B};
    <<"b">> -> {A, B * 3}
  end.



%   --- Day 23: Opening the Turing Lock ---
%   
%   Little Jane Marie just got her very first computer for Christmas from some
%   unknown benefactor. It comes with instructions and an example program, but the
%   computer itself seems to be malfunctioning. She's curious what the program
%   does, and would like you to help her run it.
%   
%   The manual explains that the computer supports two registers and six
%   instructions (truly, it goes on to remind the reader, a state-of-the-art
%   technology). The registers are named a and b, can hold any non-negative
%   integer, and begin with a value of 0. The instructions are as follows:
%   
%   hlf r sets register r to half its current value, then continues with the next instruction.
%   tpl r sets register r to triple its current value, then continues with the next instruction.
%   inc r increments register r, adding 1 to it, then continues with the next instruction.
%   jmp offset is a jump; it continues with the instruction offset away relative to itself.
%   jie r, offset is like jmp, but only jumps if register r is even ("jump if even").
%   jio r, offset is like jmp, but only jumps if register r is 1 ("jump if one", not odd).
%   All three jump instructions work with an offset relative to that instruction. The offset is always written with a prefix + or - to indicate the direction of the jump (forward or backward, respectively). For example, jmp +1 would simply continue with the next instruction, while jmp +0 would continuously jump back to itself forever.
%   
%   The program exits when it tries to run an instruction beyond the ones defined.
%   
%   For example, this program sets a to 2, because the jio instruction causes it to
%   skip the tpl instruction:
%   
%   inc a
%   jio a, +2
%   tpl a
%   inc a
%   What is the value in register b when the program in your puzzle input is
%   finished executing?
%   
%   Your puzzle answer was 170.
%   
%   --- Part Two ---
%   
%   The unknown benefactor is very thankful for releasi-- er, helping little Jane
%   Marie with her computer. Definitely not to distract you, what is the value in
%   register b after the program is finished executing if register a starts as 1
%   instead?
%   
%   Your puzzle answer was 247.
%   
%   Both parts of this puzzle are complete! They provide two gold stars: **
%   
%   At this point, all that is left is for you to admire your advent calendar.
%   
%   If you still want to see it, you can get your puzzle input.
%   
%   You can also [Share] this puzzle.
%   
