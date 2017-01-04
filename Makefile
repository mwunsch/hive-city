.SUFFIXES:

SRC := src/Campaign.elm
OBJDIR := dist
OBJECT := $(OBJDIR)/index.js

$(OBJECT): $(SRC) | $(OBJDIR)
	elm make --yes $< --output $@

$(OBJDIR):
	mkdir -p $@

.PHONY: all clean

all: $(OBJECT)

clean:
	rm -rf $(OBJDIR)
