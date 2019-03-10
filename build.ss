#!/usr/bin/env gxi
(import :std/build-script)
(defbuild-script
  `((gsc: "sdl2"
          "-verbose"
          "-cc-options" "-I/usr/local/include/SDL2 -I/usr/include/SDL2 -D_REENTRANT -D_THREAD_SAFE"
          "-ld-options" "-L/usr/local/lib -lSDL2 -lSDL2_image -lSDL2_ttf"
          "-e" "(load \"~~lib/_gambit#\")"
          )
    (ssi: "sdl2")))
