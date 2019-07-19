;;; Copyright (c) 2013 by Álvaro Castro Castilla. All Rights Reserved.
;;; Copyright (c) 2019 Aaron Marks. All Rights Reserved.
;;; SDL_Image2 Foreign Function Interface

(declare
  (block)
  (standard-bindings)
  (extended-bindings)
  (not run-time-bindings)
  (not safe))

(##include "sdl2-prelude.scm")
(c-declare "#include \"SDL_image.h\"")

(c-define-constants
 IMG_INIT_JPG
 IMG_INIT_PNG
 IMG_INIT_TIF)

;;! IMG_GetError
(define IMG_GetError
  (c-lambda () char-string "IMG_GetError"))

;;! IMG_Init
(define IMG_Init
  (c-checked zero? (c-lambda (int) int "IMG_Init")))

;;! IMG_Load
(define IMG_Load
  (c-final SDL_FreeSurface (c-checked !false? (c-lambda (char-string) SDL_Surface* "IMG_Load"))))
