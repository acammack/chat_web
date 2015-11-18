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

-module(chat_web_app).
-behaviour(application).

-export([start/2, stop/1]).

-spec start(_, _) -> {ok, pid()}.
start(_Type, _Args) ->
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/", cowboy_static, {priv_file, chat_web, "index.html"}}, % Serve the chat ui
      {"/chat", client_handler, []},
      {"/assets/[...]", cowboy_static, {priv_dir, chat_web, "assets"}} % Serve the chat ui
    ]}
  ]),
  {ok, _} = cowboy:start_http(chat_web, 100, [{port, 8080}, {ip, {127,0,0,1}}],
    [{env, [{dispatch, Dispatch}]}]).

-spec stop(_) -> ok.
stop(_State) ->
  ok.
