%% Copyright (c) 2015, Adam Cammack <adam@acammack.com>
%% 
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
%% SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
%% OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
%% CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(client_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3, websocket_handle/3, websocket_info/3, websocket_terminate/3]).

init({tcp, http}, _Req, _Opts) ->
  {upgrade, protocol, cowboy_websocket}.

websocket_init(_Transport, Req, _Opts) ->
  {ok, Req, undefined}.

websocket_handle({text, Message}, Req, User) ->
  {Reply, User2} = do_handle(jsx:decode(Message, [return_maps]), User),
  {reply, {text, Reply}, Req, User2};
websocket_handle(_Message, Req, User) ->
  {ok, Req, User}.

websocket_info({Action, Room, {U, T}}, Req, User) ->
  Message = jsx:encode(#{
    action => Action,
    room => Room,
    what => [U, T]
  }),
  {reply, {text, Message}, Req, User};
websocket_info({Action, Room, What}, Req, User) ->
  Message = jsx:encode(#{
    action => Action,
    room => Room,
    what => What,
    rooms => rooms(User)
  }),
  {reply, {text, Message}, Req, User};
websocket_info(_Info, Req, User) ->
  {ok, Req, User}.

websocket_terminate(_Reason, _Req, _User) ->
  ok.

-spec do_handle(map(), pid() | undefined) -> {binary(), pid() | undefined}.
do_handle(#{<<"nick">> := Nick, <<"password">> := Password}, undefined) ->
  case chat_core:log_on(Nick, Password) of
    {ok, U} ->
      {jsx:encode(#{action => log_on, rooms => rooms(U)}), U};
    {error, badauth} ->
      {jsx:encode(#{action => badauth}), undefined}
  end;
do_handle(#{<<"input">> := <<"/join ", Room/binary>>}, User) ->
  chat_core:join(User, Room),
  {ok(), User};
do_handle(#{<<"input">> := <<"/part">>}, User) ->
  chat_core:part(User, <<>>),
  {ok(), User};
do_handle(#{<<"input">> := <<"/part ", Room/binary>>}, User) ->
  chat_core:part(User, Room),
  {ok(), User};
do_handle(#{<<"input">> := Text}, User) ->
  case chat_core:send(User, Text) of
    ok ->
      {ok(), User};
    error ->
      {jsx:encode(#{action => error, what => <<"Cannot send chat, try joining a room">>}), User}
  end.

-spec rooms(pid()) -> [binary()].
rooms(User) ->
  chat_core:rooms(User).

-spec ok() -> binary().
ok() ->
  jsx:encode(#{action => ok}).
