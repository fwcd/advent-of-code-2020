SRCDIR = $(CURDIR)/src
OUTDIR = $(CURDIR)/out
EXE = $(OUTDIR)/day13
SRC = $(SRCDIR)/day13.c

.PHONY: all
all: $(EXE)

.PHONY: run
run: $(EXE)
	$(EXE)

$(EXE): $(SRC) | $(OUTDIR)
	$(CC) -o $(EXE) $(SRC)

$(OUTDIR):
	mkdir -p $(OUTDIR)
