# gambit
GAMBC_CC_VERBOSE=yes

CCFLAGS=-I$(GAMBIT_HOME)/include `sdl2-config --cflags`
LDFLAGS=-I$(GAMBIT_HOME)/include `sdl2-config --libs` -L$(GAMBIT_HOME)/lib

SDL_LDFLAGS=$(LDFLAGS)
TTF_LDFLAGS=$(LDFLAGS) -lSDL2_ttf

GSC=gsc # -debug -debug-location -debug-source
GSI=gsi

sdl2.o1: sdl2.scm
	$(GSC) -link -flat -o sdl2.o1.c sdl2
	$(GSC) -cc-options "$(CCFLAGS) -D___DYNAMIC" -ld-options "$(SDL_LDFLAGS)" -obj sdl2.o1.c sdl2.c
	$(CC) -shared sdl2.o sdl2.o1.o -o sdl2.o1 $(SDL_LDFLAGS)

sdl2-ttf.o1: sdl2-ttf.scm
	$(GSC) -link -flat -o sdl2-ttf.o1.c sdl2-ttf
	$(GSC) -cc-options "$(CCFLAGS) -D___DYNAMIC" -ld-options "$(TTF_LDFLAGS)" -obj sdl2-ttf.o1.c sdl2-ttf.c
	$(CC) -shared sdl2-ttf.o sdl2-ttf.o1.o -o sdl2-ttf.o1 $(TTF_LDFLAGS)

examples/life: examples/life.scm sdl2.o1 sdl2-ttf.o1
	$(GSC) -exe -cc-options "$(CCFLAGS)" -ld-options "$(LDFLAGS)" -e '(##include "~~lib/_syntax.scm")' examples/life.scm

clean:
	-rm *.c *.o1 *.o examples/life
