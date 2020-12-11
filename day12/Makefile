OUTDIR = $(CURDIR)/out
SRCDIR = $(CURDIR)/src

SRC = $(SRCDIR)/day12.pas
EXE = $(OUTDIR)/day12

.PHONY: all
all: $(EXE)

run: $(EXE)
	$(EXE)

$(EXE): $(SRC)
	fpc -FU$(OUTDIR) -FE$(OUTDIR) $(SRC)
