-module(aoc07).
-export([start/0]).

start() ->
  {ok, Fd} = file:open("aoc07.txt",[read]),
  D0 = process_signals(Fd, dict:new()),
  D1 = dict:store("b", {assign, 956}, D0),
  {V0, _D2} = get_value("a", D0), io:format("step1: a:~p~n", [V0]),
  {V1, _D3} = get_value("a", D1), io:format("step2: a:~p~n", [V1]).

process_signals(Fd, Signals) ->
  case file:read_line(Fd) of
    eof -> Signals;
    {ok, "\n"} -> process_signals(Fd, Signals);
    {ok, Line} -> process_signals(Fd, parse_line(Line, Signals))
  end.

parse_line(Line, Signals) ->
  case string:tokens(string:strip(Line, both, $\n), " ") of
    ["NOT", W1, "->", W2]        -> dict:store(W2, {'bnot', W1}, Signals);
    [W1, "->", W2]               -> dict:store(W2, {'assign', W1}, Signals);
    [W1, "AND", W2, "->", W3]    -> dict:store(W3, {'and', W1, W2}, Signals);
    [W1, "OR",  W2, "->", W3]    -> dict:store(W3, {'or', W1, W2}, Signals);
    [W1, "LSHIFT", W2, "->", W3] -> dict:store(W3, {'lshift', W1, W2}, Signals);
    [W1, "RSHIFT", W2, "->", W3] -> dict:store(W3, {'rshift', W1, W2}, Signals)
  end.

get_value(W1, SigDict) when is_integer(W1) -> {W1,SigDict};
get_value(W1, SigDict) ->
  case catch list_to_integer(W1) of
    {'EXIT', _} ->
      case catch dict:fetch(W1, SigDict) of
        {'assign', W2} -> get_value(W2, SigDict);
        {'bnot', W2} ->
            {V1, D0} = get_value(W2, SigDict),
            Int0 = bnot V1, <<Int1:16>> = <<Int0:16>>,
            {Int1, D0};
        {OP, W2, W3} when OP == 'and' orelse OP == 'or' orelse OP == 'lshift' orelse OP == 'rshift' ->
            {V1, V2, D1} = get_values(W2, W3, SigDict),
            case OP of
              'and' -> {V1 band V2, D1};
              'or'  -> {V1 bor V2, D1};
              'lshift' -> {V1 bsl V2, D1};
              'rshift' -> {V1 bsr V2, D1}
            end;
        Val -> {Val, SigDict}
      end;
    Val -> {Val, SigDict}
  end.

get_values(W1, W2, D0) ->
  F = fun(W, D) -> {V, Din0} = get_value(W, D), Din1 = dict:store(W, V, Din0), {V, Din1} end,
  {V1, D1} = F(W1, D0),
  {V2, D2} = F(W2, D1),
  {V1, V2, D2}.


%   --- Day 7: Some Assembly Required ---
%   
%   This year, Santa brought little Bobby Tables a set of wires and bitwise logic gates! Unfortunately, little Bobby is a little under the recommended age range, and he needs help assembling the circuit.
%   
%   Each wire has an identifier (some lowercase letters) and can carry a 16-bit signal (a number from 0 to 65535). A signal is provided to each wire by a gate, another wire, or some specific value. Each wire can only get a signal from one source, but can provide its signal to multiple destinations. A gate provides no signal until all of its inputs have a signal.
%   
%   The included instructions booklet describes how to connect the parts together: x AND y -> z means to connect wires x and y to an AND gate, and then connect its output to wire z.
%   
%   For example:
%   
%   123 -> x means that the signal 123 is provided to wire x.
%   x AND y -> z means that the bitwise AND of wire x and wire y is provided to wire z.
%   p LSHIFT 2 -> q means that the value from wire p is left-shifted by 2 and then provided to wire q.
%   NOT e -> f means that the bitwise complement of the value from wire e is provided to wire f.
%   Other possible gates include OR (bitwise OR) and RSHIFT (right-shift). If, for some reason, you'd like to emulate the circuit instead, almost all programming languages (for example, C, JavaScript, or Python) provide operators for these gates.
%   
%   For example, here is a simple circuit:
%   
%   123 -> x
%   456 -> y
%   x AND y -> d
%   x OR y -> e
%   x LSHIFT 2 -> f
%   y RSHIFT 2 -> g
%   NOT x -> h
%   NOT y -> i
%   After it is run, these are the signals on the wires:
%   
%   d: 72
%   e: 507
%   f: 492
%   g: 114
%   h: 65412
%   i: 65079
%   x: 123
%   y: 456
%   In little Bobby's kit's instructions booklet (provided as your puzzle input), what signal is ultimately provided to wire a?
%   
%   Your puzzle answer was 956.
%   
%   --- Part Two ---
%   
%   Now, take the signal you got on wire a, override wire b to that signal, and reset the other wires (including wire a). What new signal is ultimately provided to wire a?
%   
%   Your puzzle answer was 40149.
%   
%   Both parts of this puzzle are complete! They provide two gold stars: **
%   
%   At this point, all that is left is for you to admire your advent calendar.
%   
%   If you still want to see it, you can get your puzzle input.
%   
%   You can also [Share] this puzzle.
%   
