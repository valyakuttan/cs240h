# top level makefile

NOTES=$(shell find -iname 'l*' -type d)
clean_LIBS=$(addprefix clean_,$(NOTES))
all: $(NOTES)
clean: $(clean_NOTES)
.PHONY: force
$(NOTES): force
	make -C $@
$(clean_NOTES): force
	make -C $(patsubst clean_%,%,$@) clean
