#!/usr/bin/env gxi
(import :std/build-script)
(defbuild-script
  `((gsc: "sdl2"
          "-cc-options" "-I/usr/include/SDL2 -D_REENTRANT"
          "-ld-options" "-lSDL2 -lSDL2_image" ; -lSDL2_mixer
          "-e" "(load \"~~lib/_gambit#\")"
          )
    (ssi: "sdl2")))
