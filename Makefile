PACKAGE=erlang-smtp

include plugin-include.mk

plugin-include.mk:
	curl http://hg.rabbitmq.com/rabbitmq-public-umbrella/raw-file/default/include.mk > $@

distclean: clean
	rm -rf $(DIST_DIR)
	rm -f plugin-include.mk

debian-package: clean
	tar -cf debian-package.tar .
	mkdir build
	cd build; tar -xf ../debian-package.tar
	cd build; dpkg-buildpackage -rfakeroot -k$(SIGNING_KEY_ID)
	rm -rf build debian-package.tar

# test-compile:
# 	erlc $(ERLC_OPTS) $(wildcard test/*.erl)
# RUN_ERL_CMD=erl -pa ebin -s smtp_server -s pop3_server
# run: all
# 	$(RUN_ERL_CMD)
# run_root: all
# 	sudo $(RUN_ERL_CMD) -smtp_server listen_port 25 -pop3_server listen_port 110
