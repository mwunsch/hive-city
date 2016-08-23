.SUFFIXES:

SRC := src/Main.elm
OBJDIR := target
OBJECT := $(OBJDIR)/index.js

$(OBJECT): $(SRC) | $(OBJDIR)
	elm make $< --output $@

$(OBJDIR):
	mkdir -p $@

.PHONY: all clean

all: $(OBJECT)

clean:
	rm -rf $(OBJDIR)
