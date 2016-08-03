%%%-------------------------------------------------------------------
%%% @author hx
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. 八月 2016 下午1:37
%%%-------------------------------------------------------------------
-module(client).
-author("hx").

%% API
-export([start/5, echo/2]).
-export([enter_room/1, leave_room/1]).

-record(state, {nickname, type, player, board, socket}).

start(NickName, PlayerType, Board, SIp, SPort) ->
  Pid = spawn(fun() -> init(NickName, PlayerType, Board, SIp, SPort) end),
  {ok, Pid}.

enter_room(Pid) ->
  Pid ! enter_room,
  ok.

leave_room(Pid) ->
  Pid ! leave_room,
  ok.

echo(Pid, Msg) ->
  Pid ! {echo, Msg},
  ok.



init(NickName, PlayerType, Board, SIp, SPort) ->
  io:format("connect to ~p~n", [[SIp, SPort]]),
  {ok, Sock} = gen_tcp:connect(SIp, SPort, [binary, {active, true}, {packet, 2}]),

  Player = player:start(PlayerType, Board),
  loop(#state{nickname = NickName, type = PlayerType, player = Player, board = Board, socket = Sock}).

loop(State = #state{nickname = NickName, type = Type, player=Player, board = Board, socket = Sock}) ->
  receive
    {echo, Msg} ->
      gen_tcp:send(Sock, term_to_binary({echo, Msg})),
      loop(State);
    enter_room ->
      gen_tcp:send(Sock, term_to_binary({enter_room, NickName})),
      loop(State);
    leave_room->
      gen_tcp:send(Sock, term_to_binary({leave_room,NickName})),
      loop(State);
    {tcp, _, TcpData} ->
      case binary_to_term(TcpData) of
        {echo, Msg} ->
          io:format("ECHO: ~p~n", [Msg]);
        {notify, Msg} ->
          io:format("~s~n", [Msg]);
        {update, Move, GameState} ->
          player:update(Type, Player, GameState),
          player:display(Type, Player, GameState, Move);
        play ->
          {ok, Move} = player:get_move(Type, Player),
          io:format("my move ~p~n", [Move]),
          gen_tcp:send(Sock, term_to_binary({play, Move}));
        Unexpected->
          io:format("client receive unexpected tcp ~p~n", [Unexpected])
      end,
      loop(State)
  end.