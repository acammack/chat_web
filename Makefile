PROJECT = chat_web
DEPS = chat_core cowboy jsx

dep_chat_core = git https://github.com/acammack/chat_core master

include erlang.mk
