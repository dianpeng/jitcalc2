PWD:=$(shell pwd)
LUA:=luajit

all: calc

preprocess: src/calc.c
	$(LUA) dynasm/dynasm.lua -o src/calc.pp.c src/calc.c

calc: preprocess src/calc.c
	gcc -g src/calc.pp.c -o calc

.PHONY:clean
clean:
	find . -maxdepth 1 -executable -type f -delete
