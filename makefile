PROJECT=erlmur
DOCKER=docker-compose -f docker-compose.yml run --rm --service-ports $(PROJECT)
REBAR=$(DOCKER) rebar3

TEST_DATA_DIR = test/erlmur_SUITE_data

.PHONY: all compile test clean release $(TEST_DATA_DIR)/cert.pem $(TEST_DATA_DIR)/key.pem

all: compile test

compile:
	$(DOCKER) mkdir -p include
	$(REBAR) compile

test: dialyzer xref eunit ct
	$(REBAR) cover

eunit:
	$(REBAR) eunit

ct: $(TEST_DATA_DIR)/cert.pem $(TEST_DATA_DIR)/key.pem
	$(REBAR) ct

dialyzer:
	$(REBAR) dialyzer

xref:
	$(REBAR) xref 2>/dev/null; true

release:
	$(DOCKER) rm -Rf _build/prod
	$(REBAR) as prod compiletest/erlmur_SUITE_DATA
	$(REBAR) as prod release
	docker build --pull=true --no-cache=true --force-rm=true -t freke/$(PROJECT):0.0.1 -t freke/$(PROJECT):latest -f docker/Dockerfile_prod .

clean:
	$(REBAR) clean --all
	$(DOCKER) rm $(TEST_DATA_DIR)/cert.pem
	$(DOCKER) rm $(TEST_DATA_DIR)/key.pem
	$(DOCKER) rm -Rf _build/test/cover
	$(DOCKER) rm -Rf _build/test/logs

distclean: clean
	$(DOCKER) rm -Rf _build

upgrade:
	$(REBAR) upgrade

shell: priv/key.pem priv/cert.pem
	$(DOCKER) /bin/bash -c 'epmd -daemon; rebar3 shell --name erlmur@$$(hostname -i) --setcookie testcookie --apps erlmur'

auto: priv/key.pem priv/cert.pem
	$(REBAR) auto

priv/cert.pem: priv/key.pem

priv/key.pem:
	$(DOCKER) openssl req -x509 -newkey rsa:2048 -keyout priv/key.pem -out priv/cert.pem -days 10 -nodes -subj '/CN=localhost'

$(TEST_DATA_DIR)/cert.pem: $(TEST_DATA_DIR)/key.pem
	$(DOCKER) mkdir -p priv

$(TEST_DATA_DIR)/key.pem:
	$(DOCKER) mkdir -p $(TEST_DATA_DIR)
	$(DOCKER) openssl req -x509 -newkey rsa:2048 -keyout $(TEST_DATA_DIR)/key.pem -out $(TEST_DATA_DIR)/cert.pem -days 10 -nodes -subj '/CN=localhost'
