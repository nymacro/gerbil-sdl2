;;; Copyright (c) 2013 by Álvaro Castro Castilla. All Rights Reserved.
;;; SDL_Image2 Foreign Function Interface

(c-declare "#include \"SDL_ttf.h\"")

(c-define-type TTF_Font (struct "TTF_Font"))
(c-define-type TTF_Font* (pointer TTF_Font))

(define TTF_GetError
  (c-lambda () char-string "TTF_GetError"))

(define TTF_Init
  (c-lambda#checked zero? () int "TTF_Init"))

(define TTF_CloseFont
  (c-lambda (TTF_Font*) void "TTF_CloseFont"))

(define TTF_OpenFont
  (finalize-with
   TTF_CloseFont
   (c-lambda#checked-final !false? TTF_CloseFont (char-string int) TTF_Font* "TTF_OpenFont")))

(define TTF_RenderText_Blended
  (finalize-with
   SDL_FreeSurface
   (c-lambda#checked-final !false? SDL_FreeSurface (TTF_Font* char-string SDL_Color) SDL_Surface* "TTF_RenderText_Blended")))

(define TTF_RenderText_Solid
  (finalize-with
   SDL_FreeSurface
   (c-lambda#checked-final !false? SDL_FreeSurface (TTF_Font* char-string SDL_Color) SDL_Surface* "TTF_RenderText_Solid")))

(define TTF_RenderUTF8_Blended
  (finalize-with
   SDL_FreeSurface
   (c-lambda#checked-final !false? SDL_FreeSurface (TTF_Font* char-string SDL_Color) SDL_Surface* "TTF_RenderUTF8_Blended")))

(define TTF_RenderUTF8_Solid
  (finalize-with
   SDL_FreeSurface
   (c-lambda#checked-final !false? SDL_FreeSurface (TTF_Font* char-string SDL_Color) SDL_Surface* "TTF_RenderUTF8_Solid")))
