BIN=ebin

all: cleancompile

clean:
	rm -rf $(BIN)

deps:
	rebar get-deps

compile: deps
	rebar compile

cleancompile: clean compile

test: compile
	rebar eunit

