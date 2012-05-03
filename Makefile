OBJ=ebin/wiggle.app ebin/wiggle_app.beam ebin/wiggle_server.beam ebin/wiggle_sup.beam ebin/wiggle.beam ebin/cowboy_utils.beam ebin/wiggle_handler.beam ebin/wiggle_storage.beam ebin/wiggle_keymanager.beam ebin/wiggle_wsproxy.beam
DEPS=deps/jsx/rebar.config deps/mimetypes/rebar.config deps/erllibcloudapi/rebar.config deps/cowboy/rebar.config deps/erlydtl/rebar.config deps/alog/rebar.config deps/lhttpc/rebar.config
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
	-rm -r rel/wiggle
	-rm rel/wiggle.tar.bz2

$(DEPS):
	$(REBAR) get-deps

ebin/%.app: src/%.app.src
	$(REBAR) compile

ebin/%.beam: src/%.erl
	$(REBAR) compile

shell: all
	$(ERL) -pa $(PA) -configig standalone.configig
	-rm *.beam

FORCE:
