.PHONY: start compile

compile: ebin

run: compile
	erl -pz $(PWD)/ebin -pz deps/*/ebin -s erlmpc_app

ebin: src deps
	./rebar compile

deps:
	./rebar get-deps
