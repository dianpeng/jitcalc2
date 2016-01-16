A small integer calculator with a dynamic code generation.
It uses Luajit's awesome DynAsm and serves as a tutorial or
example for how to use this library since there's not too much
information about it.

#Contents
## calc1.c
This calculator is a very simple unsigned integer calculator, which support + - * / and variable
lookup.( I forget to implement the unary operation , so actually it still uses signed instructions )

## calc2.c
This calculator is a little fancier. It allows you to do logic, comparison , arithmatic , variable look up
and function call. Additionally it supports tenary operation as well.

#BUILD:
1. You need luajit or lua with bit operation and MUST BE on 64 bit Linux box
2. Make


# Have fun :)
