PREFIX := /opt/OpenACD/

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

.PHONY: all deps compile clean run install
