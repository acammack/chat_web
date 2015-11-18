Extended Erlang Examples - A Webchat
====================================

This webchat is designed to be a hackable and rewarding example of how to
structure and work on Erlang/OTP applications. The end result will be a project
that is interesting to explore, demonstrates The Right Way(tm), and has bugs to
fix and features to add that help ease beginners into working on complete
Erlang systems.

Running
-------

Building this project requires `git` to be in your `PATH` and either GNU make
or rebar and relx. By default, the application runs on port 8080. Change
`src/chat_web_app.erl` if a different port is desired.

To build and start with make, run `make run`. To build and start with rebar,
run `rebar get-deps; rebar compile; relx -c relx.config;
./_rel/chat_web/bin/chat_web console`.

After starting the release, point your favorite browser that supports
WebSockets (tested with Firefox and Chrome) at http://localhost:8080/ and
choose nickname and password (subsequent connections to use that nickname will
require the same password).

The available commands are:

`/join <room>` Join a room. You can be in multiple rooms at once, but your
messages will only be sent to the most recently joined room. You can `/join` a
room more than once to select it as the active room.

`/part [<room>]` Depart from a room. Defaults to the most recently joined room,
in which case the previously active room is moved back into the foreground.

Anything other than these commands will be sent to most recently joined room.


TODO
----

* Add tests with [gun](https://github.com/ninenines/gun) or possibly
  [PhantomJS](http://phantomjs.org)
* Write a guide explaining the structure and rationale behind this application
  and its dependencies.
* Write an introduction into debugging this application using observer and
  process tracing.
* Write assignments for extending functionality.
