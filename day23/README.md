# Advent of Code - Day 23

## Running

Make sure to have Common Lisp installed, e.g. `SBCL`. To run the REPL, invoke

```
rlwrap sbcl --control-stack-size 1024
```

The increased stack size is needed to create the (cyclic) linked list for part 2, which needs to hold a million items. To load and run the program, simply execute

```lisp
(load "day23.lisp")
(main)
```
