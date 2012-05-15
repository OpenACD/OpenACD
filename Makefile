SHELL := /bin/bash
TARBALL = $(abspath $(DESTDIR)OpenACD.tar.gz)

tarballfiles = attic \
       contrib \
       deps \
       doc \
       include_apps \
       misc \
       priv \
       include \
       proto_src \
       rel \
       src \
       tests \
       LICENSE \
       Makefile \
       README.markdown \
       buildcerts.sh \
       devboot \
       gen-cpx.config.sample \
       hooks.sh \
       openacd.spec \
       rebar \
       rebar.config \
       success_message \
       install.sh \
       config \
       scripts

MACHINE := $(shell uname -m)

ifeq ($(MACHINE), x86_64)
libdir = lib64
endif
ifeq ($(MACHINE), i686)
libdir = lib
endif

BINDIR=/bin
ETCDIR=/etc
LIBDIR=/$(libdir)
VARDIR=/var

OADIR=$(LIBDIR)/openacd
OALIBDIR=$(OADIR)/lib
OABINDIR=$(OADIR)/bin
OACONFIGDIR=$(ETCDIR)/openacd
OAVARDIR=$(VARDIR)/lib/openacd
OALOGDIR=$(VARDIR)/log/openacd
OADBDIR=$(OAVARDIR)/db
OAPLUGINDIR=$(OADIR)/plugin.d

all: deps compile

deps:
	./rebar get-deps update-deps

compile:
	./rebar compile generate force=1

clean:
	./rebar clean
	rm -f $(TARBALL)
	rm -rf tarball
	rm -rf OpenACD

run: compile
	./rel/openacd/bin/openacd console

install: compile
	mkdir -p $(DESTDIR)$(PREFIX)$(BINDIR)
	mkdir -p $(DESTDIR)$(PREFIX)$(OADIR)
	mkdir -p $(DESTDIR)$(PREFIX)$(OALIBDIR)
	mkdir -p $(DESTDIR)$(PREFIX)$(OABINDIR)
	mkdir -p $(DESTDIR)$(PREFIX)$(OACONFIGDIR)
	mkdir -p $(DESTDIR)$(PREFIX)$(OAVARDIR)
	mkdir -p $(DESTDIR)$(PREFIX)$(OAPLUGINDIR)
	for dep in deps/*; do \
	  ./install.sh $$dep $(DESTDIR)$(PREFIX)$(OALIBDIR) ; \
	done
	./install.sh ../OpenACD $(DESTDIR)$(PREFIX)$(OALIBDIR)
	for app in ./include_apps/*; do \
	  ./install.sh $$app $(DESTDIR)$(PREFIX)$(OALIBDIR) ; \
	done
## Plug-ins
	mkdir -p $(DESTDIR)$(PREFIX)$(OAPLUGINDIR)
## Configurations
	sed \
	-e 's|%LOG_DIR%|$(OALOGDIR)|g' \
	-e 's|%PLUGIN_DIR%|$(OAPLUGINDIR)|g' \
	./config/app.config > $(DESTDIR)$(PREFIX)$(OACONFIGDIR)/app.config
	sed \
	-e 's|%DB_DIR%|$(OADBDIR)|g' \
	./config/vm.args > $(DESTDIR)$(PREFIX)$(OACONFIGDIR)/vm.args
## Var dirs
	mkdir -p $(DESTDIR)$(PREFIX)$(OADBDIR)
	mkdir -p $(DESTDIR)$(PREFIX)$(OALOGDIR)
## Bin
#dont use DESTDIR in sed here;this is a hack to not get "rpmbuild found in installed files"
	sed \
	-e 's|%OPENACD_PREFIX%|"$(PREFIX)"|g' \
	-e 's|%LIB_DIR%|$(libdir)|g' \
	./scripts/openacd > $(DESTDIR)$(PREFIX)$(OABINDIR)/openacd
	chmod +x $(DESTDIR)$(PREFIX)$(OABINDIR)/openacd
	cp ./scripts/nodetool $(DESTDIR)$(PREFIX)$(OABINDIR)
	cd $(DESTDIR)$(PREFIX)$(BINDIR); \
	ln -sf $(PREFIX)$(OABINDIR)/openacd openacd; \
	ln -sf $(PREFIX)$(OABINDIR)/nodetool nodetool

dist: deps
	./hooks.sh pre_compile
	mkdir -p tarball/OpenACD
	rsync -aC $(tarballfiles) tarball/OpenACD
	rm -rf tarball/OpenACD/deps/protobuffs/cover.spec
	cd tarball && tar -czf $(TARBALL) OpenACD

rpm: dist
	QA_RPATHS=0x0003 rpmbuild -tb $(TARBALL)

.PHONY: all deps compile clean run install dist rpm
