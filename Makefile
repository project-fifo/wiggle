OBJ=ebin/wiggle.app ebin/wiggle_app.beam ebin/wiggle_server.beam ebin/wiggle_sup.beam ebin/wiggle.beam ebin/cowboy_utils.beam ebin/wiggle_handler.beam ebin/wiggle_storage.beam ebin/wiggle_keymanager.beam
DEPS=deps/jsx deps/lhttpc deps/alogger deps/cowboy
ERL=erl
PA=ebin deps/*/ebin
REBAR=./rebar

all: $(OBJ) $(DEPS)

rel: all FORCE
	-rm -r rel/wiggle
	cd rel; ../rebar generate

tar: rel
	cd rel; tar jcvf wiggle.tar.bz2 wiggle

clean: FORCE
	-rm -r *.beam ebin
	-rm erl_crash.dump
	-rm rel/wiggle

deps/jsx:
	$(REBAR) get-deps

deps/alogger:
	$(REBAR) get-deps

deps/lhttpc:
	$(REBAR) get-deps

ebin/%.app: src/%.app.src
	$(REBAR) compile

ebin/%.beam: src/%.erl
	$(REBAR) compile

shell: all
	$(ERL) -pa $(PA) -config standalone.config
	rm *.beam

FORCE:
