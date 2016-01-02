-module(aoc21).
-export([start/0]).


start() ->
  Weapons = [{dagger,8,4},{ss,10,5},{wh,25,6},{ls,40,7},{ga,74,8}],
  Armors  = [{zero,0,0},{lt,13,1},{cm,31,2},{sm,53,3},{bm,75,4},{pm,102,5}],
  Drings  = [{d0,0,0},{d1,25,1},{d2,50,2},{d3,100,3}],
  Arings  = [{a0,0,0},{a1,20,1},{a2,40,2},{a3,80,3}],

% Part 1
%  lists:min(lists:sort(
%  [{Ac+Bc+Cc0+Dc0+Cc1+Dc1,A,B,C0,D0,C1,D1}
%   || {A,Ac,Av} <- Weapons,
%      {B,Bc,Bv} <- Armors,
%      {C0,Cc0,Cv0} <- Drings,
%      {C1,Cc1,Cv1} <- Drings,
%      {D0,Dc0,Dv0} <- Arings,
%      {D1,Dc1,Dv1} <- Arings,
%      (Av + Bv + Cv0 + Dv0 + Cv1 + Dv1) >= 10 ] ++
%  [{Ac+Bc+Cc0+Dc0+Cc1+Dc1,A,B,C0,D0,C1,D1}
%   || {A,Ac,Av} <- Weapons,
%      {B,Bc,Bv} <- Armors,
%      {C0,Cc0,Cv0} <- Drings,
%      {C1,Cc1,Cv1} <- Drings,
%      {D0,Dc0,Dv0} <- Arings,
%      {D1,Dc1,Dv1} <- Arings,
%      (Av + Cv0 + Cv1) >= 2,
%      (Bv + Dv0 + Dv1) > 8
% ])).


% Part 2
  lists:max(
  [{Ac+Bc+Cc0+Dc0+Cc1+Dc1,A,B,C0,D0,C1,D1}
   || {A,Ac,Av} <- Weapons,
      {B,Bc,Bv} <- Armors,
      {C0,Cc0,Cv0} <- Drings,
      {C1,Cc1,Cv1} <- Drings,
      {D0,Dc0,Dv0} <- Arings,
      {D1,Dc1,Dv1} <- Arings,
      (Av + Bv + Cv0 + Dv0 + Cv1 + Dv1) < 10 
 ]).


%   --- Day 21: RPG Simulator 20XX ---
%   
%   Little Henry Case got a new video game for Christmas. It's an RPG, and he's
%   stuck on a boss. He needs to know what equipment to buy at the shop. He hands
%   you the controller.
%   
%   In this game, the player (you) and the enemy (the boss) take turns attacking.
%   The player always goes first. Each attack reduces the opponent's hit points by
%   at least 1. The first character at or below 0 hit points loses.
%   
%   Damage dealt by an attacker each turn is equal to the attacker's damage score
%   minus the defender's armor score. An attacker always does at least 1 damage.
%   So, if the attacker has a damage score of 8, and the defender has an armor
%   score of 3, the defender loses 5 hit points. If the defender had an armor score
%   of 300, the defender would still lose 1 hit point.
%   
%   Your damage score and armor score both start at zero. They can be increased by
%   buying items in exchange for gold. You start with no items and have as much
%   gold as you need. Your total damage or armor is equal to the sum of those stats
%   from all of your items. You have 100 hit points.
%   
%   Here is what the item shop is selling:
%   
%   Weapons:    Cost  Damage  Armor
%   Dagger        8     4       0
%   Shortsword   10     5       0
%   Warhammer    25     6       0
%   Longsword    40     7       0
%   Greataxe     74     8       0
%   
%   Armor:      Cost  Damage  Armor
%   Leather      13     0       1
%   Chainmail    31     0       2
%   Splintmail   53     0       3
%   Bandedmail   75     0       4
%   Platemail   102     0       5
%   
%   Rings:      Cost  Damage  Armor
%   Damage +1    25     1       0
%   Damage +2    50     2       0
%   Damage +3   100     3       0
%   Defense +1   20     0       1
%   Defense +2   40     0       2
%   Defense +3   80     0       3
%   You must buy exactly one weapon; no dual-wielding. Armor is optional, but you
%   can't use more than one. You can buy 0-2 rings (at most one for each hand). You
%   must use any items you buy. The shop only has one of each item, so you can't
%   buy, for example, two rings of Damage +3.
%   
%   For example, suppose you have 8 hit points, 5 damage, and 5 armor, and that the
%   boss has 12 hit points, 7 damage, and 2 armor:
%   
%   The player deals 5-2 = 3 damage; the boss goes down to 9 hit points.
%   The boss deals 7-5 = 2 damage; the player goes down to 6 hit points.
%   The player deals 5-2 = 3 damage; the boss goes down to 6 hit points.
%   The boss deals 7-5 = 2 damage; the player goes down to 4 hit points.
%   The player deals 5-2 = 3 damage; the boss goes down to 3 hit points.
%   The boss deals 7-5 = 2 damage; the player goes down to 2 hit points.
%   The player deals 5-2 = 3 damage; the boss goes down to 0 hit points.
%   In this scenario, the player wins! (Barely.)
%   
%   You have 100 hit points. The boss's actual stats are in your puzzle input. What
%   is the least amount of gold you can spend and still win the fight?
%   
%   Your puzzle answer was 91.
%   
%   --- Part Two ---
%   
%   Turns out the shopkeeper is working with the boss, and can persuade you to buy
%   whatever items he wants. The other rules still apply, and he still only has one
%   of each item.
%   
%   What is the most amount of gold you can spend and still lose the fight?
%   
%   Your puzzle answer was 158.
%   
%   Both parts of this puzzle are complete! They provide two gold stars: **
%   
%   At this point, all that is left is for you to admire your advent calendar.
%   
%   If you still want to see it, you can get your puzzle input.
%   
%   You can also [Share] this puzzle.
%   
