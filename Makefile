.PHONY: all deps test unit

all: rel

rel: deps
	./rebar -f compile generate

compile: deps
	./rebar compile

deps: rebar
	./rebar get-deps

clean: rebar
	./rebar clean
	rm test/*.beam

rebar:
	wget -q http://cloud.github.com/downloads/basho/rebar/rebar
	chmod u+x rebar

test:
	./rebar ct skip_deps=true

unit:
	./rebar eunit skip_deps=true
