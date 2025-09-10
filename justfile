build: format doc
	rebar3 compile

shell:
	make key
	rebar3 shell

test: xref dialyzer proper eunit ct
	rebar3 cover

format:
	rebar3 fmt -w
	rebar3 fmt -c

dialyzer:
	rebar3 dialyzer

xref:
	rebar3 xref

eunit:
	rebar3 eunit > test/eunit.log
	cat test/eunit.log | more

ct:
	make test_key
	rebar3 ct

proper:
	rebar3 proper -n 1000

upgrade:
	rebar3 upgrade --all


clean:
	rebar3 clean --all
	find {{justfile_directory()}}/_build -name "cover" -exec rm -rf {} +
	find {{justfile_directory()}}/_build -name "logs" -exec rm -rf {} +
	find {{justfile_directory()}} -name "*.coverdata" -exec rm -rf {} +
	find {{justfile_directory()}} -name "*.crashdump" -exec rm -rf {} +

clean_all: clean
	find {{justfile_directory()}}/test -name "cert.pem" -exec rm -rf {} +
	find {{justfile_directory()}}/test -name "key.pem" -exec rm -rf {} +
	find {{justfile_directory()}} -name "_build" -exec rm -rf {} +
	find {{justfile_directory()}} -name "doc" -exec rm -rf {} +
	find {{justfile_directory()}} -name "Mumble*_gpb.*" -exec rm -rf {} +
	find {{justfile_directory()}} -name "rebar.lock" -exec rm -rf {} +

doc:
	rebar3 ex_doc
	plantuml -tsvg docs/component.puml -o ../doc/images

gemini-cli:
	npx https://github.com/google-gemini/gemini-cli
