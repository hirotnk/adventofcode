-module(aoc22).
-compile(export_all).
-export([start/0]).

-define(MAX_MANA, 1500).
-define(PLAYER_MANA, 500).
-define(PLAYER_POINT, 50).
-define(BOSS_POINT, 51).
-define(BOSS_DAMAGE, 9).
-define(PLAYER_TURN, 1).
-define(BOSS_TURN, 0).
-define(MIN_MANA, 53).
-define(HARD, true). % true when part 2

start() -> play(?PLAYER_TURN, ?PLAYER_MANA, ?PLAYER_POINT, ?BOSS_POINT, 0, 0, 0).

play(?BOSS_TURN, Mana, PHitpoints, BossHitpoints, Stime, Ptime, Rtime) ->
  {Stime0, Armor, Ptime0, PHitpoints0, BossHitpoints0, Rtime0, Mana0} = update_state(Stime, Ptime, PHitpoints, BossHitpoints, Rtime, Mana),
  if
    BossHitpoints0 =< 0 -> 0;
    true ->   
      Damage = if Armor >= ?BOSS_DAMAGE -> 1; true -> (?BOSS_DAMAGE - Armor) end,
      play(?PLAYER_TURN, Mana0, PHitpoints0 - Damage, BossHitpoints0, Stime0, Ptime0, Rtime0)
  end;

play(?PLAYER_TURN, Mana, PHitpoints, _BossHitpoints, _Stime, _Ptime, _Rtime)
  when Mana < ?MIN_MANA orelse PHitpoints =< 0 -> ?MAX_MANA; % lose
play(?PLAYER_TURN, Mana, PHitpoints, BossHitpoints, Stime, Ptime, Rtime) ->
  {Stime0, _Armor, Ptime0, PHitpoints0, BossHitpoints0, Rtime0, Mana0} = update_state(Stime, Ptime, PHitpoints, BossHitpoints, Rtime, Mana),
  find_min_cost(Mana0, PHitpoints0, BossHitpoints0, Stime0, Ptime0, Rtime0).
  
find_min_cost(_Mana, PHitpoints, _BossHitpoints, _Stime, _Ptime, _Rtime) when PHitpoints =< 0 -> ?MAX_MANA;
find_min_cost(_Mana, _PHitpoints, BossHitpoints, _Stime, _Ptime, _Rtime) when BossHitpoints =< 0 -> 0;
find_min_cost(Mana, PHitpoints, BossHitpoints, Stime, Ptime, Rtime) ->
    lists:min([cast_spell(Spell, Mana, Stime, Rtime, Ptime, PHitpoints, BossHitpoints)
                || Spell <- [shield, recharge, poison, magic_missile, drain]]).

%% Some additional conditions help performance here.
cast_spell(shield, Mana, Stime, Rtime, Ptime, PlayerHitpoints, BossPoints)
  when Stime =< 0 andalso Mana >= 113 andalso PlayerHitpoints < 50 ->
    min(?MAX_MANA, 113 + play(?BOSS_TURN, Mana - 113, PlayerHitpoints, BossPoints, 7, Ptime, Rtime));
cast_spell(poison, Mana, Stime, Rtime, Ptime, PlayerHitpoints, BossPoints)
  when Ptime =< 0 andalso Mana >= 173 ->
    min(?MAX_MANA, 173 + play(?BOSS_TURN, Mana - 173, PlayerHitpoints, BossPoints, Stime, 6, Rtime));
cast_spell(recharge, Mana, Stime, Rtime, Ptime, PlayerHitpoints, BossPoints)
  when Rtime =< 0 andalso Mana >= 229 andalso Mana =< 350 ->
    min(?MAX_MANA, 229 + play(?BOSS_TURN, Mana - 229, PlayerHitpoints, BossPoints, Stime, Ptime, 5));
cast_spell(magic_missile, Mana, Stime, Rtime, Ptime, PlayerHitpoints, BossPoints)
  when Mana >= 53 ->
    min(?MAX_MANA, 53 + play(?BOSS_TURN, Mana - 53, PlayerHitpoints, BossPoints - 4, Stime, Ptime, Rtime));
cast_spell(drain, Mana, Stime, Rtime, Ptime, PlayerHitpoints, BossPoints)
  when Mana >= 73 ->
    min(?MAX_MANA, 73 + play(?BOSS_TURN, Mana - 73, PlayerHitpoints + 2, BossPoints - 2, Stime, Ptime, Rtime));
cast_spell(_, _Mana, _Stime, _Rtime, _Ptime, _PlayerHitpoints, _BossPoints) -> ?MAX_MANA.
    
update_state(Stime, Ptime, PHitpoints, BossHitpoints, Rtime, Mana) ->
  {Stime0, Armor}   = if Stime > 0 -> {Stime - 1, 7}; true -> {0, 0} end, 
  {Ptime0, BossHitpoints0} = if Ptime > 0 -> {Ptime - 1, BossHitpoints - 3}; true -> {0, BossHitpoints} end,
  {Rtime0, Mana0}   = if Rtime > 0 -> {Rtime - 1, Mana + 101}; true -> {0, Mana} end,
  PHitpoints0 = if ?HARD == true -> PHitpoints - 1; true -> PHitpoints  end,
  {Stime0, Armor, Ptime0, PHitpoints0, BossHitpoints0, Rtime0, Mana0}.




%   --- Day 22: Wizard Simulator 20XX ---
%   
%   Little Henry Case decides that defeating bosses with swords and stuff is
%   boring. Now he's playing the game with a wizard. Of course, he gets stuck on
%   another boss and needs your help again.
%   
%   In this version, combat still proceeds with the player and the boss taking
%   alternating turns. The player still goes first. Now, however, you don't get any
%   equipment; instead, you must choose one of your spells to cast. The first
%   character at or below 0 hit points loses.
%   
%   Since you're a wizard, you don't get to wear armor, and you can't attack
%   normally. However, since you do magic damage, your opponent's armor is ignored,
%   and so the boss effectively has zero armor as well. As before, if armor (from a
%   spell, in this case) would reduce damage below 1, it becomes 1 instead - that
%   is, the boss' attacks always deal at least 1 damage.
%   
%   On each of your turns, you must select one of your spells to cast. If you
%   cannot afford to cast any spell, you lose. Spells cost mana; you start with 500
%   mana, but have no maximum limit. You must have enough mana to cast a spell, and
%   its cost is immediately deducted when you cast it. Your spells are Magic
%   Missile, Drain, Shield, Poison, and Recharge.
%   
%   Magic Missile costs 53 mana. It instantly does 4 damage.  Drain costs 73 mana.
%   It instantly does 2 damage and heals you for 2 hit points.  Shield costs 113
%   mana. It starts an effect that lasts for 6 turns. While it is active, your
%   armor is increased by 7.  Poison costs 173 mana. It starts an effect that lasts
%   for 6 turns. At the start of each turn while it is active, it deals the boss 3
%   damage.  Recharge costs 229 mana. It starts an effect that lasts for 5 turns.
%   At the start of each turn while it is active, it gives you 101 new mana.
%   Effects all work the same way. Effects apply at the start of both the player's
%   turns and the boss' turns. Effects are created with a timer (the number of
%   turns they last); at the start of each turn, after they apply any effect they
%   have, their timer is decreased by one. If this decreases the timer to zero, the
%   effect ends. You cannot cast a spell that would start an effect which is
%   already active. However, effects can be started on the same turn they end.
%   
%   For example, suppose the player has 10 hit points and 250 mana, and that the
%   boss has 13 hit points and 8 damage:
%   
%   -- Player turn --
%   - Player has 10 hit points, 0 armor, 250 mana
%   - Boss has 13 hit points
%   Player casts Poison.
%   
%   -- Boss turn --
%   - Player has 10 hit points, 0 armor, 77 mana
%   - Boss has 13 hit points
%   Poison deals 3 damage; its timer is now 5.
%   Boss attacks for 8 damage.
%   
%   -- Player turn --
%   - Player has 2 hit points, 0 armor, 77 mana
%   - Boss has 10 hit points
%   Poison deals 3 damage; its timer is now 4.
%   Player casts Magic Missile, dealing 4 damage.
%   
%   -- Boss turn --
%   - Player has 2 hit points, 0 armor, 24 mana
%   - Boss has 3 hit points
%   Poison deals 3 damage. This kills the boss, and the player wins.
%   Now, suppose the same initial conditions, except that the boss has 14 hit points instead:
%   
%   -- Player turn --
%   - Player has 10 hit points, 0 armor, 250 mana
%   - Boss has 14 hit points
%   Player casts Recharge.
%   
%   -- Boss turn --
%   - Player has 10 hit points, 0 armor, 21 mana
%   - Boss has 14 hit points
%   Recharge provides 101 mana; its timer is now 4.
%   Boss attacks for 8 damage!
%   
%   -- Player turn --
%   - Player has 2 hit points, 0 armor, 122 mana
%   - Boss has 14 hit points
%   Recharge provides 101 mana; its timer is now 3.
%   Player casts Shield, increasing armor by 7.
%   
%   -- Boss turn --
%   - Player has 2 hit points, 7 armor, 110 mana
%   - Boss has 14 hit points
%   Shield's timer is now 5.
%   Recharge provides 101 mana; its timer is now 2.
%   Boss attacks for 8 - 7 = 1 damage!
%   
%   -- Player turn --
%   - Player has 1 hit point, 7 armor, 211 mana
%   - Boss has 14 hit points
%   Shield's timer is now 4.
%   Recharge provides 101 mana; its timer is now 1.
%   Player casts Drain, dealing 2 damage, and healing 2 hit points.
%   
%   -- Boss turn --
%   - Player has 3 hit points, 7 armor, 239 mana
%   - Boss has 12 hit points
%   Shield's timer is now 3.
%   Recharge provides 101 mana; its timer is now 0.
%   Recharge wears off.
%   Boss attacks for 8 - 7 = 1 damage!
%   
%   -- Player turn --
%   - Player has 2 hit points, 7 armor, 340 mana
%   - Boss has 12 hit points
%   Shield's timer is now 2.
%   Player casts Poison.
%   
%   -- Boss turn --
%   - Player has 2 hit points, 7 armor, 167 mana
%   - Boss has 12 hit points
%   Shield's timer is now 1.
%   Poison deals 3 damage; its timer is now 5.
%   Boss attacks for 8 - 7 = 1 damage!
%   
%   -- Player turn --
%   - Player has 1 hit point, 7 armor, 167 mana
%   - Boss has 9 hit points
%   Shield's timer is now 0.
%   Shield wears off, decreasing armor by 7.
%   Poison deals 3 damage; its timer is now 4.
%   Player casts Magic Missile, dealing 4 damage.
%   
%   -- Boss turn --
%   - Player has 1 hit point, 0 armor, 114 mana
%   - Boss has 2 hit points
%   Poison deals 3 damage. This kills the boss, and the player wins.  You start
%   with 50 hit points and 500 mana points. The boss's actual stats are in your
%   puzzle input. What is the least amount of mana you can spend and still win the
%   fight? (Do not include mana recharge effects as "spending" negative mana.)
%   
%   Your puzzle answer was 900.
%   
%   --- Part Two ---
%   
%   On the next run through the game, you increase the difficulty to hard.
%   
%   At the start of each player turn (before any other effects apply), you lose 1
%   hit point. If this brings you to or below 0 hit points, you lose.
%   
%   With the same starting stats for you and the boss, what is the least amount of
%   mana you can spend and still win the fight?
%   
%   Your puzzle answer was 1216.
%   
%   Both parts of this puzzle are complete! They provide two gold stars: **
%   
%   At this point, all that is left is for you to admire your advent calendar.
%   
%   If you still want to see it, you can get your puzzle input.
%   
%   You can also [Share] this puzzle.
%   
