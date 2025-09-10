TEST_DATA_DIR = test/erlmur_SUITE_data

key: priv/cert.pem priv/key.pem

test_key: $(TEST_DATA_DIR)/cert.pem $(TEST_DATA_DIR)/key.pem





priv/cert.pem: priv/key.pem

priv/key.pem:
	mkdir -p priv
	openssl req -x509 -newkey rsa:2048 -keyout priv/key.pem -out priv/cert.pem -days 10 -nodes -subj '/CN=localhost'

$(TEST_DATA_DIR)/cert.pem: $(TEST_DATA_DIR)/key.pem
	mkdir -p priv

$(TEST_DATA_DIR)/key.pem:
	mkdir -p $(TEST_DATA_DIR)
	openssl req -x509 -newkey rsa:2048 -keyout $(TEST_DATA_DIR)/key.pem -out $(TEST_DATA_DIR)/cert.pem -days 10 -nodes -subj '/CN=localhost'
