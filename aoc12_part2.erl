-module(aoc12_part2).
-export([start/0]).

% This solution uses mochijson2 in mochiweb.
% https://github.com/mochi/mochiweb
start() ->
  L = get_data_list(),
  parse(L).

get_data_list() ->
  {ok, Bin} = file:read_file("aoc12.txt"),
  mochijson2:decode(Bin).

parse([]) -> 0;
parse({struct, A}) when is_list(A) ->
  case length([K || {K,<<"red">>} <- A]) > 0  of
    true -> 0;
    false -> lists:foldl(fun(E,Cnt) -> parse(E) + Cnt end, 0, A)
  end;
parse([H|T]) when is_integer(H) -> H + parse(T);
parse([{struct,A}|T]) ->
  parse({struct,A}) + parse(T);
parse([H|T]) -> parse(H) + parse(T);

parse({_, {struct,A}}) -> parse({struct,A});
parse({_,B}) when is_integer(B) -> B;
parse({_,B}) when is_list(B) -> parse(B);
parse({_,_}) -> 0;
parse(_) -> 0.

