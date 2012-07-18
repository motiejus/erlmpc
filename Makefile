.PHONY: run compile
REBAR_URL=https://github.com/downloads/Motiejus/rebar/rebar

run: compile
	erl -pz $(PWD)/ebin -pz deps/*/ebin -s erlmpc_app

compile: rebar
	./rebar get-deps compile

rebar:
	wget $(REBAR_URL) || curl $(REBAR_URL) > rebar
	chmod a+x rebar
