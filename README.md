erlmur
======

mumble (murmur) server written in erlang.

currently only with the most basic functionality, and all settings hardcoded.


ssl certificate
===============

create a self signed certificate

$ openssl genrsa -out key.pem 1024
$ openssl req -new -key key.pem -out request.pem
$ openssl x509 -req -days 30 -in request.pem -signkey key.pem -out server.pem

to run the erlmur server the created "key.pem" and "server.pem" and place them in the project root dir


start erlmur
============

from project root dir

$ erl -pa ebin -pa deps/protobuffs/ebin
1> erlmur:start().
