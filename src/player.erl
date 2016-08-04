%%%-------------------------------------------------------------------
%%% @author hx
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. 八月 2016 下午4:36
%%%-------------------------------------------------------------------
-module(player).
-author("hx").

%% API
-export([start/2, update/3, display/4, get_move/2]).


start(human, Board) ->
  human:start(Board);
start(mcts, Board) ->
  mcts:start(Board, 1000, 1.4).

update(human, Player, GameState) ->
  human:update(Player, GameState);
update(mcts, Player, GameState) ->
  mcts:update(Player, GameState).

display(human, Player, GameState, Move) ->
  human:display(Player, GameState, Move);
display(mcts, Player, GameState, Move) ->
  mcts:display(Player, GameState, Move).

get_move(human, Player) ->
  human:get_move(Player);
get_move(mcts, Player) ->
  mcts:get_move(Player).