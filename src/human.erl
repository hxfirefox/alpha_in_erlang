%%%-------------------------------------------------------------------
%%% @author hx
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. 八月 2016 下午4:29
%%%-------------------------------------------------------------------
-module(human).
-author("hx").

%% API
-export([start/1, update/2, display/3, get_move/1]).


start(Board) ->
  put(board, Board),
  human.

update(_Player, _GameState) ->
  void.

display(_Player, GameState, Move) ->
  Board = get(board),
  io:format("~ts~n", [Board:display(GameState, Move)]).

get_move(Player) ->
  Move = io:get_line("Your move (R C r c):"),
  Board = get(board),
  case Board:parse(Move) of
    none ->
      get_move(Player);
    P ->
      {ok, P}
  end.