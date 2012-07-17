.PHONY: start compile deps

run:
	erl -pz $(PWD)/ebin -pz apps/*/ebin deps/*/ebin -s erlmpc_app

compile:
	./rebar compile

deps:
	./rebar get-deps
