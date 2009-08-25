SOURCE_DIR=src
EBIN_DIR=ebin
INCLUDE_DIR=include
INCLUDES=$(wildcard $(INCLUDE_DIR)/*.hrl)
SOURCES=$(wildcard $(SOURCE_DIR)/*.erl)
TARGETS=$(patsubst $(SOURCE_DIR)/%.erl, $(EBIN_DIR)/%.beam,$(SOURCES))
APPFILES=$(patsubst $(SOURCE_DIR)/%.app, $(EBIN_DIR)/%.app, $(wildcard $(SOURCE_DIR)/*.app))
ERLC_OPTS=-I $(INCLUDE_DIR) -o $(EBIN_DIR) -Wall +debug_info # +native -v
DIST_DIR=dist
SIGNING_KEY_ID=E96F1FA7

all: $(EBIN_DIR) $(TARGETS) $(APPFILES)

$(EBIN_DIR)/%.beam: $(SOURCE_DIR)/%.erl $(INCLUDES)
	erlc $(ERLC_OPTS) $<

$(EBIN_DIR)/%.app: $(SOURCE_DIR)/%.app
	cp $< $@

$(EBIN_DIR):
	mkdir -p $@

clean:
	rm -rf $(EBIN_DIR)

dist: all
	mkdir -p $(DIST_DIR)
	cp -r ebin src $(DIST_DIR)

distclean: clean
	rm -rf $(DIST_DIR)
	find . -name '*~' -exec rm {} \;

debian-package: clean
	tar -cf debian-package.tar .
	mkdir build
	cd build; tar -xf ../debian-package.tar
	cd build; dpkg-buildpackage -rfakeroot -k$(SIGNING_KEY_ID)
	rm -rf build debian-package.tar

test-compile: $(EBIN_DIR)
	erlc $(ERLC_OPTS) $(wildcard test/*.erl)
	cp test/*.app $(EBIN_DIR)

RUN_ERL_CMD=erl -pa ebin -s smtp_server_test -s pop3_server_test

run: all test-compile
	$(RUN_ERL_CMD)

run_root: all test-compile
	sudo $(RUN_ERL_CMD) -smtp_server_test listen_port 25 -pop3_server_test listen_port 110
