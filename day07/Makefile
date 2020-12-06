OUTDIR = $(CURDIR)/out
EXE = $(OUTDIR)/gcc-debug/day07
SRC = day07.kk

.PHONY: all
all: $(EXE)

.PHONY: run
run: $(EXE)
	$(EXE)

$(EXE): $(SRC)
	koka -c $(SRC) -o $(OUTDIR)
