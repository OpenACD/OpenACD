PREFIX := /opt/OpenACD/
TARBALL = $(abspath $(DESTDIR)OpenACD.tar.gz)

all: deps compile

deps:
	./rebar get-deps update-deps force=1
	git submodule init
	git submodule update

compile:
	./rebar compile generate force=1

clean:
	./rebar clean

run: compile
	./rel/openacd/bin/openacd console

install: compile
	mkdir -p ${PREFIX}
	cp -r rel/openacd/* ${PREFIX}
	mkdir -p ${PREFIX}/plugin.d/deps

dist: deps
	GIT_UPDATE_DISABLED=1 ./pre_compile.sh
	git archive --format tar --prefix OpenACD/ HEAD > $(TARBALL:.gz=)
	tar -rf $(TARBALL:.gz=) \
		OpenACD/deps/* \
		OpenACD/priv/www/contrib/dojo/dojo \
		OpenACD/priv/www/contrib/dojo/dojox \
		OpenACD/priv/www/contrib/dojo/dijit
	cat $(TARBALL:.gz=) | gzip > $(TARBALL)

rpm: dist
	QA_RPATHS=0x0003 rpmbuild -tb $(TARBALL)

.PHONY: all deps compile clean run install dist rpm
