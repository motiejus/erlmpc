.PHONY: start compile deps

compile:
	./rebar compile

run: compile
	erl -pz $(PWD)/ebin -pz deps/*/ebin -s erlmpc_app

deps:
	./rebar get-deps
