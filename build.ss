#!/usr/bin/env gxi
(import :std/make)

(define lib-build-spec
  '((gsc: "sdl2"
          "-cc-options" "-I/usr/local/include/SDL2 -I/usr/include/SDL2 -D_REENTRANT -D_THREAD_SAFE"
          "-ld-options" "-L/usr/local/lib -lSDL2"
          "-e" "(load \"~~lib/_gambit#\")")
    (gsc: "sdl2-ttf"
          "-cc-options" "-I/usr/local/include/SDL2 -I/usr/include/SDL2 -D_REENTRANT -D_THREAD_SAFE"
          "-ld-options" "-L/usr/local/lib -lSDL2_ttf"
          "-e" "(load \"~~lib/_gambit#\")")
    (gsc: "sdl2-image"
          "-cc-options" "-I/usr/local/include/SDL2 -I/usr/include/SDL2 -D_REENTRANT -D_THREAD_SAFE"
          "-ld-options" "-L/usr/local/lib -lSDL2_image"
          "-e" "(load \"~~lib/_gambit#\")")
    (ssi: "sdl2")))

(define bin-build-spec
  '((static-exe: "examples/life.ss")))

(def deps-build-spec
  (append lib-build-spec bin-build-spec))

(def srcdir
  (path-normalize (path-directory (this-source-file))))

(def (main . args)
  (match args
    (["deps"]
     (cons-load-path srcdir)
     (let ((build-deps (make-depgraph/spec deps-build-spec)))
       (call-with-output-file "build-deps" (cut write build-deps <>))))
    (["lib"]
     (let ((depgraph (call-with-input-file "build-deps" read)))
       (make srcdir: srcdir
             bindir: srcdir
             optimize: #t
             debug: 'src
             static: #t
             depgraph: depgraph
             prefix: "nymacro"
             lib-build-spec)))
    (["bin"]
     (let ((depgraph (call-with-input-file "build-deps" read)))
       (make srcdir: srcdir
             bindir: srcdir
             optimize: #t
             debug: #f
             static: #t
             depgraph: depgraph
             prefix: "nymacro"
             bin-build-spec)))
    ([]
     (make "lib")
     (make "bin"))))

;; (import :std/build-script)
;; (defbuild-script
;;   `((gsc: "sdl2"
;;           "-cc-options" "-I/usr/local/include/SDL2 -I/usr/include/SDL2 -D_REENTRANT -D_THREAD_SAFE"
;;           "-ld-options" "-L/usr/local/lib -lSDL2"
;;           "-e" "(load \"~~lib/_gambit#\")")
;;     (gsc: "sdl2-ttf"
;;           "-cc-options" "-I/usr/local/include/SDL2 -I/usr/include/SDL2 -D_REENTRANT -D_THREAD_SAFE"
;;           "-ld-options" "-L/usr/local/lib -lSDL2_ttf"
;;           "-e" "(load \"~~lib/_gambit#\")")
;;     (gsc: "sdl2-image"
;;           "-cc-options" "-I/usr/local/include/SDL2 -I/usr/include/SDL2 -D_REENTRANT -D_THREAD_SAFE"
;;           "-ld-options" "-L/usr/local/lib -lSDL2_image"
;;           "-e" "(load \"~~lib/_gambit#\")")
;;     (ssi: "sdl2")))
    
