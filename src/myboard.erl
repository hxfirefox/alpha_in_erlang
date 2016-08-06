%%%-------------------------------------------------------------------
%%% @author hx
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. 八月 2016 上午10:35
%%%-------------------------------------------------------------------
-module(myboard).
-author("hx").

%% API
-export([start/0]).


start() ->
  S = lists:duplicate(20, 0) ++ [none, none, 1], list_to_tuple(S).

state_value(I, State) ->
  element(I + 1, State).

set_state_val(I, V, State) ->
  setelement(I + 1, State, V).

player(State) ->
  element(23, State).