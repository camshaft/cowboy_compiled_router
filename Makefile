PROJECT = cowboy_compiled_router

# dependencies

DEPS = cowlib privdir fast_key

dep_cowlib = git https://github.com/ninenines/cowlib.git 1.0.1
dep_privdir = git https://github.com/camshaft/privdir.git
dep_fast_key = git https://github.com/camshaft/fast_key.git

include erlang.mk

test: eunit

ebin/%.beam: test/%.erl
	$(call compile_erl,$<)

eunit: app ebin/$(PROJECT)_test.beam
	@erl \
	  -noshell \
	  -pa ebin \
	  -eval "eunit:test($(PROJECT)_test, [verbose])" \
	  -s init stop

.PHONY: eunit repl
