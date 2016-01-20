LUA:=luajit
UDIS:=-ludis86
all: calc1 calc2 calc3

calc1: src/calc1.c
	$(LUA) dynasm/dynasm.lua -o src/calc1.pp.c src/calc1.c
	gcc -O3 -g src/calc1.pp.c $(UDIS) -o calc1

calc2: src/calc2.c
	$(LUA) dynasm/dynasm.lua -o src/calc2.pp.c src/calc2.c
	gcc -O3 -g src/calc2.pp.c $(UDIS) -o calc2

calc3: src/calc3.c
	$(LUA) dynasm/dynasm.lua -o src/calc3.pp.c src/calc3.c
	gcc -O3 -g src/calc3.pp.c $(UDIS) -o calc3

.PHONY:clean
clean:
	find . -maxdepth 1 -executable -type f -delete
	rm src/*.pp.c
