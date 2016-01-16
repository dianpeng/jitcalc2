PWD:=$(shell pwd)
LUA:=luajit

all: calc1 calc2

calc1: src/calc1.c
	$(LUA) dynasm/dynasm.lua -o src/calc1.pp.c src/calc1.c
	gcc -O3 -g src/calc1.pp.c -o calc1

calc2: src/calc2.c
	$(LUA) dynasm/dynasm.lua -o src/calc2.pp.c src/calc2.c
	gcc  -g src/calc2.pp.c -o calc2

.PHONY:clean
clean:
	find . -maxdepth 1 -executable -type f -delete
	rm src/*.pp.c
