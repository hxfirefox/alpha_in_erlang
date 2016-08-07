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

-record(state, {board = board,
  max_time = 2000, % milliseconds
  exploration_factor = 1.4,
  plays_wins, % ets, key={player, game_state}
  %           vale={plays::integer(),
  %                 wins::integer()}
  game_state = []}).


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
    max_time = MaxTime,
    exploration_factor = ExplorationFactor,
    plays_wins = PlaysWins},
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

handle_call({update, GameState}, State = #state{game_state = GSs}) ->
  {reply, ok, State#state{game_state = [GameState | GSs]}};
handle_call({display, GameState, Move}, State = #state{board = Board}) ->
  io:format("~ts~n", [Board:display(GameState, Move)]),
  {reply, ok, State};
handle_call(get_move, State = #state{board = Board, game_state = GSs}) ->
  GS = hd(GSs), % GSs current state
  {Player, LegalStates} = player_legal_states(Board, GS),
  NextMove =
    case LegalStates of
      [] ->
        illegal;
      [{Move, _}] ->
        Move;
      _ ->
        {Games, MaxDepth, Time} %% MaxDepth-existed selection set
          = run_simulation(Player, LegalStates, State),
        io:format("Games: ~p Time: ~pms~n", [Games, Time]),
        io:format("Maximum depth searched: ~p~n", [MaxDepth]),
        %% [{move,percent,wins,plays}]
        Stats = make_stats(Player, LegalStates, State#state.plays_wins), %% update list
        SortedStats = lists:reverse(lists:keysort(2, Stats)),
        [io:format("~p: ~.2f% (~p / ~p)~n", [Move, Percent, Wins, Plays]) || {Move, Percent, Wins, Plays} <- SortedStats],
        [{Move, _, _, _} | _] = SortedStats,
        Move
    end,
  {reply, {ok, NextMove}, State}.

player_legal_states(Board, CurGameState) ->
  Player = Board:current_player(CurGameState),
  Moves = Board:legal_moves(CurGameState),
  LegalStates = [{Move, Board:next_state(CurGameState, Move)} || Move <- Moves],
  {Player, LegalStates}.

get_plays_wins(Tid, Player, GameState) ->
  case lookup(Tid, {Player, GameState}) of
    none ->
      [0.0, 0.0];
    {Plays, Wins} ->
      [100 * Wins / Plays, Wins, Plays]
  end.


lookup(Tid, Key) ->
  case ets:lookup(Tid, Key) of
    [{_, Value}] ->
      Value;
    [] ->
      none
  end.

insert(Tid, Key, Value) ->
  ets:insert(Tid, {Key, Value}).

run_simulation(Player, LegalStates, State) ->
  BeginTime = os:timestamp(),
  run_simulation(Player, LegalStates, State, {BeginTime, 0, 0}).

run_simulation(Player, LegalStates, State, {BeginTime, Games, MaxDepth}) ->
  TimeComsumed = timer:now_diff(os:timestamp(), BeginTime) div 1000,
  case TimeComsumed < State#state.max_time of
    true ->
      {Winner, Expand, NeedUpdateds, Depth}
        = random_game(Player, LegalStates, State),
      propagate_back(Winner, Expand, NeedUpdateds, State#state.plays_wins),
      run_simulation(Player, LegalStates, State, {BeginTime, Games + 1, max(Depth, MaxDepth)});
    false ->
      {Games, MaxDepth, TimeComsumed}
  end.

random_game(Player, LegalStates, State = #state{board = Board}) ->
  <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
  random:seed({A, B, C}),
  MaxMoves = Board:max_moves(),
  random_game(Player, LegalStates, 1, MaxMoves, {none, [], 0}, State).

random_game(_, _, IterCount, MaxMoves, {Expand, NeedUpdates, MaxDepth}, _) when IterCount =:= MaxMoves + 1 ->
  {draw, Expand, NeedUpdates, MaxDepth};
random_game(_, [], _, _, {Expand, NeedUpdates, MaxDepth}, _) ->
  {draw, Expand, NeedUpdates, MaxDepth};
random_game(Player, LegalStats, IterCount, MaxMoves, {Expand, NeedUpdateds, MaxDepth}, State) ->
  {GS, Existed} = select_one(Player, LegalStats, State),
  {Expand2, NeedUpdateds2, MaxDepth2} =
    case {Expand, Existed} of % existed-existed point
      {none, false} ->
        {{Player, GS}, NeedUpdateds, IterCount}; % expansion
      {_, true} ->
        {Expand, [{Player, GS} | NeedUpdateds], MaxDepth};
      {_, false} ->
        {Expand, NeedUpdateds, MaxDepth}
    end,
  case get_winner(State#state.board, GS) of
    on_going ->
      {Player2, LegalStats2}
        = player_legal_states(State#state.board, GS),
      random_game(Player2, LegalStats2, IterCount + 1, MaxMoves, {Expand2, NeedUpdateds2, MaxDepth2}, State);
    Winner ->
      {Winner, Expand2, NeedUpdateds2, MaxDepth2}
  end.

%% return:{GameState, Existed}
select_one(Player, LegalStats, #state{exploration_factor = EF, plays_wins = PlaysWins}) ->
  GSs = [I || {_, I} <- LegalStats],
  RandomGS = choice(GSs),
  {RandomGS, lookup(PlaysWins, {Player, RandomGS}) =/= none}.

propagate_back(Winner, none, NeedUpdateds, PlaysWins) ->
  update_play_wins(Winner, NeedUpdateds, PlaysWins);
propagate_back(Winner, Expand, NeedUpdateds, PlaysWins) ->
  insert(PlaysWins, Expand, {0, 0}),
  update_play_wins(Winner, [Expand | NeedUpdateds], PlaysWins).

update_play_wins(Winner, Updateds, PlaysWins) ->
  [begin {Ps, Ws} = lookup(PlaysWins, Key),
  Ws2 = if
          Winner =:= Player -> Ws + 1;
          true -> Ws
        end,
  insert(PlaysWins, Key, {Ps + 1, Ws2}) end || {Player, _} = Key <- Updateds].

%% 1|2|draw|on_going
get_winner(Board, GS) ->
  Board:winner(GS).

choice(L) ->
  lists:nth(random:uniform(length(L)), L).

%% return: [{move, percent, wins, plays}]
make_stats(Player, LegalStates, PlaysWins) ->
  [list_to_tuple([Move | get_plays_wins(PlaysWins, Player, GameState)]) || {Move, GameState} <- LegalStates].