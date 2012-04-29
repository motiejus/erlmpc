.PHONY: start compile

start: compile
	erl -pz $(PWD)/ebin -pz deps/*/ebin -s erlmpc_app

compile: ebin

ebin: src deps
	./rebar compile

deps:
	./rebar get-deps
