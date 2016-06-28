PROJECT=erlmur
DOCKER=docker-compose -f docker-compose.yml run --rm --service-ports $(PROJECT)
REBAR=$(DOCKER) rebar3

.PHONY: all compile test clean

all: compile test

compile:
	$(REBAR) compile

test: dialyzer eunit ct
	$(REBAR) cover

eunit:
	$(REBAR) eunit

ct:
	$(REBAR) ct

dialyzer:
	$(REBAR) dialyzer

release:
	$(DOCKER) rm -Rf _build/prod
	$(REBAR) as prod compile
	$(REBAR) as prod release
	docker build --pull=true --no-cache=true --force-rm=true -t freke/$(PROJECT):0.0.1 -t freke/$(PROJECT):latest -f docker/Dockerfile_prod .

clean:
	$(REBAR) clean --all
	$(DOCKER) rm -Rf _build/test/cover
	$(DOCKER) rm -Rf _build/test/logs

distclean: clean
	$(DOCKER) rm -Rf _build

upgrade:
	$(REBAR) upgrade

shell:
	$(REBAR) shell
