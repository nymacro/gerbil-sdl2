#!/usr/bin/env gxi
(import :std/build-script)
(defbuild-script
  `((gsc: "sdl2"
          "-debug"
          "-debug-location"
          "-debug-environments"
          "-debug-source"
          "-keep-c"
          "-verbose"
          "-cc-options" "-I/usr/include/SDL2 -D_REENTRANT"
          "-ld-options" "-lSDL2 -lSDL2_image -lSDL2_ttf"
          "-e" "(load \"~~lib/_gambit#\")"
          )
    (ssi: "sdl2")))
