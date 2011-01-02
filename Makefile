PREFIX := /opt/OpenACD/

compile:
	rm -rf rel/openacd
	@./rebar get-deps update-deps compile generate

clean:
	@./rebar clean

run: compile
	./rel/openacd/bin/openacd console

install: compile
	mkdir -p ${PREFIX}
	cp -r rel/openacd/* ${PREFIX}

.PHONY: compile clean run prefix
