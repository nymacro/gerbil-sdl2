# gambit
GAMBC_CC_VERBOSE=yes

CCFLAGS=-O2 -I$(GAMBIT_HOME)/include `sdl2-config --cflags`
LDFLAGS=-I$(GAMBIT_HOME)/include `sdl2-config --libs` -L$(GAMBIT_HOME)/lib

SDL_LDFLAGS=$(LDFLAGS)
TTF_LDFLAGS=$(LDFLAGS) -lSDL2_ttf
IMG_LDFLAGS=$(LDFLAGS) -lSDL2_image

GSC=gsc # -debug -debug-location -debug-source -track-scheme
GSI=gsi

all: sdl2.o1 sdl2-ttf.o1 sdl2-image.o1 examples/life

sdl2.o1: sdl2.scm sdl2-prelude.scm
	$(GSC) -link -flat -o sdl2.o1.c sdl2
	$(GSC) -cc-options "$(CCFLAGS) -D___DYNAMIC" -ld-options "$(SDL_LDFLAGS)" -obj sdl2.o1.c sdl2.c
	$(CC) -shared sdl2.o sdl2.o1.o -o sdl2.o1 $(SDL_LDFLAGS)

sdl2-ttf.o1: sdl2-ttf.scm sdl2-prelude.scm
	$(GSC) -link -flat -o sdl2-ttf.o1.c sdl2-ttf
	$(GSC) -cc-options "$(CCFLAGS) -D___DYNAMIC" -ld-options "$(TTF_LDFLAGS)" -obj sdl2-ttf.o1.c sdl2-ttf.c
	$(CC) -shared sdl2-ttf.o sdl2-ttf.o1.o -o sdl2-ttf.o1 $(TTF_LDFLAGS)

sdl2-image.o1: sdl2-image.scm sdl2-prelude.scm
	$(GSC) -link -flat -o sdl2-image.o1.c sdl2-image
	$(GSC) -cc-options "$(CCFLAGS) -D___DYNAMIC" -ld-options "$(IMG_LDFLAGS)" -obj sdl2-image.o1.c sdl2-image.c
	$(CC) -shared sdl2-image.o sdl2-image.o1.o -o sdl2-image.o1 $(IMG_LDFLAGS)

examples/life: examples/life.scm examples/life_shared.scm sdl2.o1 # sdl2-ttf.o1
	$(GSC) -exe -cc-options "$(CCFLAGS)" -ld-options "$(LDFLAGS)" -e '(##include "~~lib/_syntax.scm")' examples/life.scm

gsi-life:
	$(GSI) -e '(##include "~~lib/_syntax.scm")' examples/life.scm

clean:
	-rm *.c
	-rm *.o*
	-rm examples/life
