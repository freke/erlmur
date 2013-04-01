# erlmur

[![Build Status](https://travis-ci.org/freke/erlmur.png?branch=master)](https://travis-ci.org/freke/erlmur)

mumble (murmur) server written in erlang.

currently only with the most basic functionality, and all settings hardcoded.


## ssl certificate

create a self signed certificate


	$ openssl genrsa -out key.pem 1024
	$ openssl req -new -key key.pem -out request.pem
	$ openssl x509 -req -days 30 -in request.pem -signkey key.pem -out server.pem

## start erlmur

to run the erlmur server you first have to move "key.pem" and "server.pem" to the project root dir

then from project root dir

	$ erl -pa ebin -pa deps/protobuffs/ebin -pa deps/record_info/ebin
	1> erlmur:start().
