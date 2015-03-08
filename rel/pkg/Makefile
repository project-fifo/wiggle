VERSION=0.6.2
COMPONENT_INTERNAL=wiggle
COMPONENT=fifo-wiggle
DEPS="erlang>=16.1.2"

include ../../deps/fifo_utils/priv/pkg.mk

.PHONY: prepare

prepare:
	-rm -r $(STAGE_DIR)/$(COMPONENT)
	cp -r ../$(COMPONENT_INTERNAL) $(STAGE_DIR)/$(COMPONENT)

package: prepare $(FILE).tgz

clean: clean-pkg
	-rm *.tgz

