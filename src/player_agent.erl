%%%-------------------------------------------------------------------
%%% @author hx
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. 八月 2016 下午1:47
%%%-------------------------------------------------------------------
-module(player_agent).
-author("hx").

-record(state, {sock}).
%% API
-export([init/1, handle_tcp_data/2, handle_info/2]).


init(Socket) ->
  {ok, #state{sock = Socket}}.

handle_tcp_data(TcpData, State) ->
  case binary_to_term(TcpData) of
    {echo, Msg} ->
      send_message({echo, Msg}, State);
    {enter_room, NickName} ->
      room:enter(self(), NickName);
    {leave_room, _NickName} ->
      room:leave(self());
    {play, Move} ->
      room:play(self(), Move)
  end,
  {ok, State}.

handle_info(Msg, State) ->
  send_message(Msg, State),
  {ok, State}.

send_message(Message, State) ->
  TcpData = term_to_binary(Message),
  gen_tcp:send(State#state.sock, TcpData),
  ok.