# scmvm

## NAME

`scmvm` - a scheme virtual machine. And assembler.

## HOW TO USE IT
```
  $ scheme-of-choice             # scheme-of-choice means csi
REPL> (load "defs.scm")
REPL> (load "vm.scm")
```
## DESCRIPTION

`scmvm` is a very basic virtual machine for experimenting with compilation techniques. It is designed to run in Chicken Scheme (*defs.scm* is actually a copy of my *.csirc*), but modifying it to run under other Schemes is trivial.

`scmvm` instruction code isn't really a bytecode but a *symbolic code* - that is, it is made of Lisp atoms. This enables extending the instruction set with great degree of ease.

`scmvm` comes with an assembler. In the repository you can find the examples of assembly code (note that it is mostly a symbolic code with labels instead of raw addresses).

## FUNCTIONS

(**vm:assemble-from-file** *source-file* *origin-offset*) - assembles a program from *source-file* and stores it in the VM memory at address *origin-offset*.

(**vm:run**) - runs a program until HALT.

(**vm:step**) - executes a single instruction and stops.

(**vm:dump-registers**) - examine the content of registers.

(**vm:reset**) - resets the VM. Resetting `scmvm` might not be the thing you think it is.

(**vm:load-program** *vector*) - loads a *vector* with symbolic code into VM's memory.

## BUGS

The README is misleading. The code is, too.
