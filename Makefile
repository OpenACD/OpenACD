compile:
	rm -rf rel/openacd
	@./rebar get-deps update-deps compile generate

clean:
	@./rebar clean

run: compile
	./rel/openacd/bin/openacd console

.PHONY: compile clean
