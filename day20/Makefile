SRCDIR = $(CURDIR)/src
OUTDIR = $(CURDIR)/out

SRC = $(SRCDIR)/day20.cpp
EXE = $(OUTDIR)/day20

.PHONY: all
all: $(EXE)

.PHONY: run
run: $(EXE)
	@$(EXE)

$(EXE): $(SRC) $(OUTDIR)
	clang++ -O2 -std=c++2a -Wall -o $(EXE) $(SRC)

$(OUTDIR):
	mkdir -p $(OUTDIR)
