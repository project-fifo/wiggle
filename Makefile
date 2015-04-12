REBAR = $(shell pwd)/rebar

.PHONY: deps rel stagedevrel version

all: cp-hooks deps compile

cp-hooks:
	cp hooks/* .git/hooks

quick-xref:
	$(REBAR) xref skip_deps=true -r

quick-test:
	$(REBAR) skip_deps=true eunit

version:
	@echo "$(shell git symbolic-ref HEAD 2> /dev/null | cut -b 12-)-$(shell git log --pretty=format:'%h, %ad' -1)" > wiggle.version

version_header: version
	@echo "-define(VERSION, <<\"$(shell cat wiggle.version)\">>)." > apps/wiggle/src/wiggle_version.hrl

compile: version_header
	$(REBAR) compile

deps:
	$(REBAR) get-deps

clean:
	$(REBAR) clean
	[ -d apps/wiggle/ebin] && rm -r apps/wiggle/ebin || true
	make -C rel/pkg clean

distclean: clean devclean relclean
	$(REBAR) delete-deps

test: all
	$(REBAR) skip_deps=true xref -r
	$(REBAR) skip_deps=true eunit

rel: all zabbix
	-rm -r rel/wiggle/share
	$(REBAR) generate

relclean:
	rm -rf rel/wiggle

package: rel
	make -C rel/pkg package

console: all
	erl -pa deps/*/ebin apps/*/ebin -s wiggle -config standalone.config

zabbix:
	sh generate_zabbix_template.sh

###
### Docs
###
docs:
	$(REBAR) skip_deps=true doc

##
## Developer targets
##

xref: compile
	@$(REBAR) xref skip_deps=true -r

stage : rel
	$(foreach dep,$(wildcard deps/* wildcard apps/*), rm -rf rel/wiggle/lib/$(shell basename $(dep))-* && ln -sf $(abspath $(dep)) rel/wiggle/lib;)

##
## Dialyzer
##
APPS = kernel stdlib sasl erts ssl tools os_mon runtime_tools crypto inets \
	xmerl webtool snmp public_key mnesia eunit syntax_tools compiler
COMBO_PLT = $(HOME)/.wiggle_combo_dialyzer_plt

check_plt: deps compile
	dialyzer --check_plt --plt $(COMBO_PLT) --apps $(APPS) \
		deps/*/ebin apps/*/ebin

build_plt: deps compile
	dialyzer --build_plt --output_plt $(COMBO_PLT) --apps $(APPS) \
		deps/*/ebin apps/*/ebin

dialyzer: deps compile
	@echo
	@echo Use "'make check_plt'" to check PLT prior to using this target.
	@echo Use "'make build_plt'" to build PLT prior to using this target.
	@echo
	@sleep 1
	dialyzer -Wno_return --plt $(COMBO_PLT) deps/*/ebin apps/*/ebin | grep -v -f dialyzer.mittigate


cleanplt:
	@echo
	@echo "Are you sure?  It takes about 1/2 hour to re-build."
	@echo Deleting $(COMBO_PLT) in 5 seconds.
	@echo
	sleep 5
	rm $(COMBO_PLT)
