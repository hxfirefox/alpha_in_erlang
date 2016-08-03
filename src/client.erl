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

-record(state, {socket}).
%% API
-export([start/2, echo/2]).


start(SIp, Sport) ->
  Pid = spawn(fun() -> init(SIp, Sport) end),
  {ok, Pid}.

echo(Pid, Msg) ->
  Pid ! {echo, Msg},
  ok.



init(SIp, SPort) ->
  io:format("connect to ~p~n", [[SIp, SPort]]),
  {ok, Sock} = gen_tcp:connect(SIp, SPort, [binary, {active, true}, {packet, 2}]),
  loop(#state{socket = Sock}).

loop(State = #state{socket = Sock}) ->
  receive
    {echo, Msg} ->
      gen_tcp:send(Sock, term_to_binary({echo, Msg})),
      loop(State);
    {tcp, _, TcpData} -> case binary_to_term(TcpData) of
                           {echo, Msg} ->
                             io:format("ECHO: ~p~n", [Msg])
                         end,
      loop(State)
  end.