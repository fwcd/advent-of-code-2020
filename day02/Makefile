.PHONY: all
all: out/main

out:
	mkdir -p out

out/main: src/main.m | out
	gcc -o out/main src/main.m `gnustep-config --objc-flags` -lgnustep-base -lobjc
