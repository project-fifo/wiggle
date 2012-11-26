APP_NAME=wiggle
APP_DIR=apps/$(APP_NAME)
OBJ=$(shell ls $(APP_DIR)/src/*.erl | sed -e 's/\.erl$$/.beam/' | sed -e 's;^$(APP_DIR)/src;$(APP_DIR)/ebin;g') $(shell ls $(APP_DIR)/src/*.app.src | sed -e 's/\.src$$//g' | sed -e 's;^$(APP_DIR)/src;$(APP_DIR)/ebin;g')
DEPS=$(shell cat rebar.config  |sed -e 's/%.*//'| sed -e '/{\(\w\+\), [^,]\+, {\w\+, [^,]\+, {[^,]\+, [^}]\+}}},\?/!d' | sed -e 's;{\(\w\+\), [^,]\+, {\w\+, [^,]\+, {[^,]\+, [^}]\+}}},\?;deps/\1/rebar.config;')
ERL=erl
PA=$(shell pwd)/$(APP_DIR)/ebin
ERL_LIBS=`pwd`/deps/
REBAR=./rebar

.PHONY: rel clean package deps

all: $(DEPS) $(OBJ)

deps:
	$(REBAR) get-deps
	$(REBAR) update-deps

rel: all
	-rm -r rel/$(APP_NAME)
	cd rel; ../rebar generate
echo:
	echo $(DEPS)

tar: rel
	cd rel; tar jcvf $(APP_NAME).tar.bz2 $(APP_NAME)

test:
	$(REBAR) skip_deps=true eunit

clean:
	$(REBAR) clean
	make -C rel/pkg clean
	-rm *.beam erl_crash.dump
	-rm -r rel/$(APP_NAME)
	-rm rel/$(APP_NAME).tar.bz2

$(DEPS):
	$(REBAR) get-deps
	$(REBAR) compile

$(APP_DIR)/ebin/%.app: $(APP_DIR)/src/%.app.src
	$(REBAR) compile

$(APP_DIR)/ebin/%.beam: $(APP_DIR)/src/%.erl
	$(REBAR) compile

shell: all
	ERL_LIBS="$(ERL_LIBS)" $(ERL) -pa $(PA) -config standalone -sname $(APP_NAME)
	[ -f *.beam ] && rm *.beam || true
	[ -f erl_crash.dump ] && rm erl_crash.dump || true

package: rel
	make -C rel/pkg package
