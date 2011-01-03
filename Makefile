PREFIX := /opt/OpenACD/

compile:
	@./rebar get-deps update-deps compile generate force=1

clean:
	@./rebar clean

run: compile
	./rel/openacd/bin/openacd console

install: compile
	mkdir -p ${PREFIX}
	cp -r rel/openacd/* ${PREFIX}

.PHONY: compile clean run prefix
