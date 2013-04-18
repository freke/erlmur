compile: get-deps
	rebar compile

test: compile
	rebar eunit skip_deps=true
	rebar ct skip_deps=true

get-deps:
	rebar get-deps

clean:
	rebar clean

APPS = kernel stdlib sasl erts ssl crypto inets public_key
COMBO_PLT = $(HOME)/.erlmur_combo_dialyzer_plt

check_plt: compile
	dialyzer --check_plt --plt $(COMBO_PLT) --apps $(APPS) deps/*/ebin

build_plt: compile
	dialyzer --build_plt --output_plt $(COMBO_PLT) --apps $(APPS) deps/*/ebin

dialyzer: compile
	@echo
	@echo Use "'make check_plt'" to check PLT prior to using this target.
	@echo Use "'make build_plt'" to build PLT prior to using this target.
	@echo
	@sleep 1
	dialyzer -Wno_return -Wrace_conditions --plt $(COMBO_PLT) deps/*/ebin | fgrep -v -f ./dialyzer.ignore-warnings

cleanplt:
	@echo
	@echo "Are you sure? It takes about 1/2 hour to re-build."
	@echo Deleting $(COMBO_PLT) in 5 seconds.
	@echo
	sleep 5
	rm $(COMBO_PLT)

xref: compile
	rebar xref skip_deps=true
