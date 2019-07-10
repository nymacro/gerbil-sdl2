GAMBC_CC_VERBOSE=yes

CCFLAGS=-I$(GAMBIT_HOME)/include `sdl2-config --cflags`
LDFLAGS=-I$(GAMBIT_HOME)/include `sdl2-config --libs` -L$(GAMBIT_HOME)/lib -lSDL2

GSC=gsc -debug -debug-location -debug-source

sdl2.c: sdl2.scm
	$(GSC) -c -cc-options "$(CCFLAGS) -D___LIBRARY" -ld-options "$(LDFLAGS)" sdl2.scm

# how to generate a object with correct bindings???
sdl2.o: sdl2.c
	$(CC) -c $(CCFLAGS) -o sdl2.o sdl2.c

life: life.scm sdl2.c
	$(GSC) -exe -cc-options "$(CCFLAGS)" -ld-options "$(LDFLAGS)" -e '(##include "~~lib/syntax-case.scm")' sdl2.c life.scm

clean:
	-rm *.c *.o1 *.o
