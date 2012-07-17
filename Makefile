.PHONY: run compile deps

run: deps compile
	erl -pz $(PWD)/ebin -pz deps/*/ebin -s erlmpc_app

compile:
	./rebar compile

deps:
	./rebar get-deps
