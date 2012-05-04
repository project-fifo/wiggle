OBJ=$(shell ls src/*.erl | sed -e 's/\.erl$$/.beam/' | sed -e 's/^src/ebin/g') $(shell ls src/*.app.src | sed -e 's/\.src$$//g' | sed -e 's/^src/ebin/g')
DEPS=$(shell cat rebar.config  |sed -e 's/%.*//'| sed -e '/{\(\w\+\), [^,]\+, {\w\+, [^,]\+, {[^,]\+, [^}]\+}}},\?/!d' | sed -e 's;{\(\w\+\), [^,]\+, {\w\+, [^,]\+, {[^,]\+, [^}]\+}}},\?;deps/\1/rebar.config;')
ERL=erl
PA=$(shell pwd)/ebin
ERL_LIBS=`pwd`/deps/
REBAR=./rebar
APP_NAME=wiggle

all: $(OBJ) $(DEPS)

rel: all FORCE
	-rm -r rel/$(APP_NAME)
	cd rel; ../rebar generate
echo:
	echo $(DEPS)

tar: rel
	cd rel; tar jcvf $(APP_NAME).tar.bz2 $(APP_NAME)

clean: FORCE
	$(REBAR) clean
	-rm *.beam erl_crash.dump
	-rm -r rel/$(APP_NAME)
	-rm rel/$(APP_NAME).tar.bz2

$(DEPS):
	$(REBAR) get-deps

ebin/%.app: src/%.app.src
	$(REBAR) compile

ebin/%.beam: src/%.erl
	$(REBAR) compile

shell: all
	ERL_LIBS="$(ERL_LIBS)" $(ERL) -pa $(PA) -config standalone
	[ -f *.beam ] && rm *.beam || true

FORCE:
