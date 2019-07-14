# gambit
GAMBC_CC_VERBOSE=yes

CCFLAGS=-I$(GAMBIT_HOME)/include `sdl2-config --cflags`
LDFLAGS=-I$(GAMBIT_HOME)/include `sdl2-config --libs` -L$(GAMBIT_HOME)/lib -lSDL2

GSC=gsc # -debug -debug-location -debug-source
GSI=gsi

sdl2.o1:
	$(GSC) -link -flat -o sdl2.o1.c sdl2
	$(GSC) -cc-options "$(CCFLAGS) -D___DYNAMIC" -ld-options "$(LDFLAGS)" -obj sdl2.o1.c sdl2.c
	$(CC) -shared sdl2.o sdl2.o1.o -o sdl2.o1 $(LDFLAGS)

examples/life: examples/life.scm sdl2.o1
	$(GSC) -exe -cc-options "$(CCFLAGS)" -ld-options "$(LDFLAGS)" -e '(##include "~~lib/_syntax.scm")' examples/life.scm

clean:
	-rm *.c *.o1 *.o examples/life
