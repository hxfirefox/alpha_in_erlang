%%%-------------------------------------------------------------------
%%% @author hx
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 八月 2016 上午11:12
%%%-------------------------------------------------------------------
-module(mcts).
-author("hx").

%% API
-export([start/0, start/3]).
-export([get_move/1, update/2, display/3]).

-record(state, {board = board, max_time = 2000, exploration_factor = 1.4, plays_wins, game_state = []}).


start() ->
  start(board, 5000, 1.4).

start(Board, MaxTime, ExplorationFactor) ->
  spawn(fun() -> init([Board, MaxTime, ExplorationFactor]) end).

update(Pid, GameState) ->
  call(Pid, {update, GameState}).

display(Pid, GameState, Move) ->
  call(Pid, {display, GameState, Move}).

get_move(Pid) ->
  call(Pid, get_move).

init([Board, MaxTime, ExplorationFactor]) ->
  PlaysWins = ets:new(plays_wins, [set, protected, {read_concurrency, true}]),
  State = #state{board = Board,
    max_time = MaxTime, exploration_factor = ExplorationFactor, plays_wins = PlaysWins},
  loop(State).


loop(State) ->
  receive
    {call, Ref, From, Msg} ->
      case handle_call(Msg, State) of
        {reply, Reply, NewState} ->
          From ! {Ref, Reply},
          loop(NewState);
        stop ->
          stop
      end
  end.

call(Pid, Msg) ->
  Ref = make_ref(),
  Pid ! {call, Ref, self(), Msg},
  receive
    {Ref, Result} ->
      Result
  end.

handle_call(Msg, State) ->
  {reply, ok, State}.