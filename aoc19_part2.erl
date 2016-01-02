-module(aoc19_part2).
-compile(export_all).
-export([start/0]).

% Solution from:
% https://www.reddit.com/r/adventofcode/comments/3xflz8/day_19_solutions/
start() ->
  MM = "CRnSiRnCaPTiMgYCaPTiRnFArSiThFArCaSiThSiThPBCaCaSiRnSiRnTiTiMgArPBCaPMgYPTiRnFArFArCaSiRnBPMgArPRnCaPTiRnFArCaSiThCaCaFArPBCaCaPTiTiRnFArCaSiRnSiAlYSiThRnFArArCaSiRnBFArCaCaSiRnSiThCaCaCaFYCaPTiBCaSiThCaSiThPMgArSiRnCaPBFYCaCaFArCaCaCaCaSiThCaSiRnPRnFArPBSiThPRnFArSiRnMgArCaFYFArCaSiRnSiAlArTiTiTiTiTiTiTiRnPMgArPTiTiTiBSiRnSiAlArTiTiRnPMgArCaFYBPBPTiRnSiRnMgArSiThCaFArCaSiThFArPRnFArCaSiRnTiBSiThSiRnSiAlYCaFArPRnFArSiThCaFArCaCaSiThCaCaCaSiRnPRnCaFArFYPMgArCaPBCaPBSiRnFYPBCaFArCaSiAl",
  CountTokens = fun (E, Cnt) when E >= $A andalso E =< $Z -> Cnt + 1; (E, Cnt) -> Cnt end,
  CP =
    fun CountParens("Rn" ++ T) -> 1 + CountParens(T)
       ;CountParens("Ar" ++ T) -> 1 + CountParens(T)
       ;CountParens([_|T]) -> CountParens(T)
       ;CountParens([]) -> 0
    end,
  lists:foldl(CountTokens, 0, MM) - CP(MM) - 2 * length([Y || $Y = Y <- MM]) - 1.

