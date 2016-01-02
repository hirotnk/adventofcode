-module(aoc15).
-export([start/0]).

start() ->
  ets:new(ingredients, [set, named_table]),
  Ingredients = get_data_list(),
  Res =
  lists:map(fun(E) ->
    lists:foldl(
      fun(I,Acc) ->
          case lists:keysearch(I, 1, Acc) of
            false -> [{I,1}|Acc];
            {value, {I,Cnt}} -> lists:keyreplace(I, 1, Acc, {I, Cnt+1})
          end
      end, E, Ingredients) end, combi(100 - length(Ingredients), Ingredients)),
  MaxScore =
    lists:max(lists:map(
        fun(Mix) ->
          Score = {C,D,F,T,Cal} =
          lists:foldl(
            fun({I,Cnt},{Cin,Din,Fin,Tin,Calin}) ->
              {Capacity, Durability, Flavor, Texture, Calories} = ets:lookup_element(ingredients, I, 2),
              {Capacity * Cnt + Cin,
               Durability * Cnt + Din,
               Flavor * Cnt + Fin,
               Texture * Cnt + Tin,
               Calories * Cnt + Calin}
            end, {0,0,0,0,0}, Mix),
          case Score of
            {C,D,F,T,_Cal} when C < 0 orelse D < 0 orelse F < 0 orelse T < 0 -> 0;
            {C,D,F,T,500} -> C * D * F * T;
            _ -> 0
          end
        end, Res)),
  io:format("Score:~p~n", [MaxScore]),
  ets:delete(ingredients).

get_data_list() ->
  {ok, Bin} = file:read_file("aoc15.txt"),
  [ begin
      [Ingredient, _, Capacity, _, Durability, _, Flavor, _, Texture, _, Calories]
        = binary:split(B, [<<" ">>, <<", ">>], [global]),
      ets:insert(ingredients,
        {Ingredient,
          {binary_to_integer(Capacity), binary_to_integer(Durability),
           binary_to_integer(Flavor), binary_to_integer(Texture), binary_to_integer(Calories)}}),
      Ingredient
    end
      || B <- binary:split(Bin, [<<"\n">>], [global]), size(B) > 0].

combi(0, _Ingredients) -> [[]];
combi(Target, Ingredients) ->
  lists:flatmap(
    fun(E) ->
      lists:map(
        fun(EE) ->
          case lists:keysearch(E, 1, EE) of
            false -> [{E,1}|EE];
            {value, {E,Cnt}} -> lists:keyreplace(E, 1, EE, {E, Cnt+1})
          end
        end,
        combi(Target-1, [I||I <- Ingredients, I =< E]))
    end,
    Ingredients).



    
%   --- Day 15: Science for Hungry People ---
%   
%   Today, you set out on the task of perfecting your milk-dunking cookie recipe. All you have to do is find the right balance of ingredients.
%   
%   Your recipe leaves room for exactly 100 teaspoons of ingredients. You make a list of the remaining ingredients you could use to finish the recipe (your puzzle input) and their properties per teaspoon:
%   
%   capacity (how well it helps the cookie absorb milk)
%   durability (how well it keeps the cookie intact when full of milk)
%   flavor (how tasty it makes the cookie)
%   texture (how it improves the feel of the cookie)
%   calories (how many calories it adds to the cookie)
%   You can only measure ingredients in whole-teaspoon amounts accurately, and you have to be accurate so you can reproduce your results in the future. The total score of a cookie can be found by adding up each of the properties (negative totals become 0) and then multiplying together everything except calories.
%   
%   For instance, suppose you have these two ingredients:
%   
%   Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
%   Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3
%   Then, choosing to use 44 teaspoons of butterscotch and 56 teaspoons of cinnamon (because the amounts of each ingredient must add up to 100) would result in a cookie with the following properties:
%   
%   A capacity of 44*-1 + 56*2 = 68
%   A durability of 44*-2 + 56*3 = 80
%   A flavor of 44*6 + 56*-2 = 152
%   A texture of 44*3 + 56*-1 = 76
%   Multiplying these together (68 * 80 * 152 * 76, ignoring calories for now) results in a total score of 62842880, which happens to be the best score possible given these ingredients. If any properties had produced a negative total, it would have instead become zero, causing the whole score to multiply to zero.
%   
%   Given the ingredients in your kitchen and their properties, what is the total score of the highest-scoring cookie you can make?
%   
%   Your puzzle answer was 21367368.
%   
%   --- Part Two ---
%   
%   Your cookie recipe becomes wildly popular! Someone asks if you can make another recipe that has exactly 500 calories per cookie (so they can use it as a meal replacement). Keep the rest of your award-winning process the same (100 teaspoons, same ingredients, same scoring system).
%   
%   For example, given the ingredients above, if you had instead selected 40 teaspoons of butterscotch and 60 teaspoons of cinnamon (which still adds to 100), the total calorie count would be 40*8 + 60*3 = 500. The total score would go down, though: only 57600000, the best you can do in such trying circumstances.
%   
%   Given the ingredients in your kitchen and their properties, what is the total score of the highest-scoring cookie you can make with a calorie total of 500?
%   
%   Your puzzle answer was 1766400.
%   
%   Both parts of this puzzle are complete! They provide two gold stars: **
%   
%   At this point, all that is left is for you to admire your advent calendar.
%   
%   If you still want to see it, you can get your puzzle input.
%   
%   You can also [Share] this puzzle.
%     
