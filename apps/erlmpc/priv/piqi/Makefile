# This Makefile is for developer's convenience. It allows to recompile only one
# application using rebar without recompiling all the dependencies.


# this is necessary, because sometimes the $ERL_LIBS and $REBAR_DEPS_DIR
# variables exported by Rebar will be lost
MAKEFLAGS = -e


all:
	cd ..; ./rebar compile skip_deps=true


clean:
	cd ..; ./rebar clean skip_deps=true

