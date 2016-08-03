%%%-------------------------------------------------------------------
%%% @author hx
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. 八月 2016 下午11:53
%%%-------------------------------------------------------------------
-module(game).
-author("hx").

%% API
-export([start/0]).


start() ->
  room:start(board),
  tcp_server:start(8081, player_agent),
  ok.