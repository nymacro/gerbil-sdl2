;;; -*- Scheme -*-
;;; Copyright (c) 2013 by Álvaro Castro Castilla. All Rights Reserved.
;;; Copyright (c) 2018-2019 Aaron Marks. All Rights Reserved.
;;; SDL2_ttf Foreign Function Interface

(declare
  (block)
  (standard-bindings)
  (extended-bindings)
  (not run-time-bindings)
  (not safe))

(##include "sdl2-prelude.scm")

(c-declare "#include \"SDL_ttf.h\"")

(c-define-type TTF_Font (struct "TTF_Font"))
(c-define-type TTF_Font* (pointer TTF_Font))

(define TTF_GetError
  (c-lambda () char-string "TTF_GetError"))

(define TTF_Init
  (c-checked zero? (c-lambda () int "TTF_Init")))

(define TTF_CloseFont
  (c-lambda (TTF_Font*) void "TTF_CloseFont"))

(define TTF_OpenFont
  (c-final TTF_CloseFont (c-checked !false? (c-lambda (char-string int) TTF_Font* "TTF_OpenFont"))))

(define TTF_RenderText_Blended
  (c-final SDL_FreeSurface (c-checked !false? (c-lambda (TTF_Font* char-string SDL_Color) SDL_Surface* "TTF_RenderText_Blended"))))

(define TTF_RenderText_Solid
  (c-final SDL_FreeSurface (c-checked !false? (c-lambda (TTF_Font* char-string SDL_Color) SDL_Surface* "TTF_RenderText_Solid"))))

(define TTF_RenderUTF8_Blended
  (c-final SDL_FreeSurface (c-checked !false? (c-lambda (TTF_Font* char-string SDL_Color) SDL_Surface* "TTF_RenderUTF8_Blended"))))

(define TTF_RenderUTF8_Solid
  (c-final SDL_FreeSurface (c-checked !false? (c-lambda (TTF_Font* char-string SDL_Color) SDL_Surface* "TTF_RenderUTF8_Solid"))))
