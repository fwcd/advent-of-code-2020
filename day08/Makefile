OUTDIR = $(CURDIR)/out
EXE = $(OUTDIR)/gcc-debug/day08
SRC = day08.kk

.PHONY: all
all: $(EXE)

.PHONY: run
run: $(EXE)
	$(EXE)

$(EXE): $(SRC)
	koka -c $(SRC) -o $(OUTDIR)
