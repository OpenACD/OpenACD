compile:
	@./rebar get-deps update-deps compile

clean:
	@./rebar clean

.PHONY: compile clean
