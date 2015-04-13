REBAR = $(shell pwd)/rebar

.PHONY: deps rel stagedevrel version

all: deps rel

cp-hooks:
	cp hooks/* .git/hooks

version: deps compile
	cp deps/wiggle/wiggle.version .

deps:
	$(REBAR) get-deps

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean
	make -C rel/pkg clean

distclean: clean devclean relclean
	$(REBAR) delete-deps

rel: compile zabbix
	-rm -r rel/wiggle/share
	$(REBAR) generate
relclean:
	rm -rf rel/wiggle

package: rel
	make -C rel/pkg package

zabbix:
	sh generate_zabbix_template.sh
