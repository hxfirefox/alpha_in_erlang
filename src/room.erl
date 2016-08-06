%%%-------------------------------------------------------------------
%%% @author hx
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. 八月 2016 下午2:28
%%%-------------------------------------------------------------------
-module(room).
-author("hx").

%% API
-export([start/1]).
-export([enter/2, leave/1, play/2, show/0]).
-export([reset/0]).

-record(state, {board, status = waiting, current_player = none, players = [], game_state}).


start(Board) ->
  Pid = spawn(fun() -> init(Board) end),
  register(room, Pid),
  {ok, Pid}.

enter(Pid, NickName) ->
  room ! {enter, Pid, NickName}.

leave(Pid) ->
  room ! {leave, Pid}.

play(Pid, Move) ->
  room ! {play, Pid, Move}.

reset() ->
  room ! reset.

init(Board) ->
  <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
  random:seed({A, B, C}),
  loop(#state{board = Board}).

loop(State = #state{status = waiting, board = Board, players = Players}) ->
  receive
    {enter, Pid, NickName} ->
      case Players of
        [] ->
          notify_user(Pid, greeting(NickName)),
          Ref = erlang:monitor(process, Pid),
          loop(State#state{players = [{Pid, NickName, Ref}]});
        [{Pid, _, _}] ->
          loop(state);
        [{Pid2, _, _}] ->
          notify_user(Pid, greeting(NickName)),
          notify_user(Pid2, enter_room(NickName)),
          Ref = erlang:monitor(process, Pid),
          NewPlayers = [{Pid, NickName, Ref} | Players],
          First = select_player(NewPlayers),
          GameState = Board:start(),
          self() ! begin_game,
          loop(State#state{status = playing, game_state = GameState, current_player = First, players = NewPlayers})
      end;
    {leave, Pid} ->
      case lists:keyfind(Pid, 1, Players) of
        {Pid, _, Ref} ->
          NewPlayers = lists:keydelete(Pid, 1, Players),
          erlang:demonitor(Ref),
          loop(State#state{players = NewPlayers});
        _ ->
          loop(State)
      end;
    {'DOWN', _, process, Pid, Reason} ->
      io:format("~p down @waiting for: ~p~n", [Pid, Reason]),
      self() ! {leave, Pid},
      loop(State);
    Unexpected ->
      io:format("unexpected @waiting ~p~n", [Unexpected]),
      loop(State)
  end;
loop(State = #state{status = playing, current_player = {Current, NickName}, players = Players, board = Board, game_state = GameState}) ->
  receive
    {enter, Pid, NickName} ->
      loop(State);
    {leave, Pid} ->
      case lists:keyfind(Pid, 1, Players) of
        {Pid, NickName, Ref} ->
          NewPlayers = [{Pid2, NickName2, _}]
            = lists:keydelete(Pid, 1, Players),
          erlang:demonitor(Ref),
          loop(State#state{status = waiting, current_player = none, players = NewPlayers});
        _ ->
          loop(state)
      end;
    begin_game ->
      {Next, _} = next_player(Current, Players),
      update(Current, GameState),
      update(Next, GameState),
      play(Current),
      loop(State);
    {play, Current, Move} ->
      case Board:is_legal(GameState, Move) of
        false ->
          play(Current),
          loop(State);
        true ->
          GameState2 = Board:next_state(GameState, Move),
          NextPlayer = {Next, _} = next_player(Current, Players),
          update(Current, Move, GameState2),
          update(Next, Move, GameState2),
          case Board:winner(GameState2) of
            on_going ->
              play(Next),
              loop(State#state{game_state = GameState2, current_player = NextPlayer});
            draw ->
              loop(State#state{status = waiting, players = [], current_player = none});
            _ ->
              {_, WN} = State#state.current_player,
              [notify_user(P, io:format("winner is ~p~n", [WN])) || {P, _} <- State#state.players],
              loop(State#state{status = waiting, players = [], current_player = none})
          end
      end;
    {'DOWN', _, process, Pid, Reason} ->
      io:format("~p down @playing for: ~p~n", [Pid, Reason]),
      self() ! {leave, Pid},
      loop(State);
    Unexpected ->
      io:format("unexpected @playing ~p~n", [Unexpected]),
      loop(State)
  end.

select_player(Players) ->
  N = random:uniform(2),
  {Pid, NickName, _} = lists:nth(N, Players),
  {Pid, NickName}.

show() ->
  erlang:error(not_implemented).

next_player(Pid, [{Pid, _, _}, {Pid2, NickName, _}]) ->
  {Pid2, NickName};
next_player(Pid, [{Pid2, NickName, _}, {Pid, _, _}]) ->
  {Pid2, NickName}.

update(Pid, GameState) ->
  Pid ! {update, none, GameState}.
update(Pid, Move, GameState) ->
  Pid ! {update, Move, GameState}.

play(Pid) ->
  Pid ! play.

notify_user(Pid, Msg) ->
  Pid ! {notify, Msg}.

greeting(NickName) ->
  io:format("Welcom ~p~n", [NickName]).

enter_room(NickName) ->
  io:format("begin game with ~p~n", [NickName]).
