;;;; gerbil-sdl2 life
;;;; Copyright (C) 2019 Aaron Marks. All Rights Reserved.

(import :std/crypto) ; for random-bytes
(import :nymacro/sdl2)
(import :std/format)
(import :std/sugar)
(import :std/iter)
(import :gerbil/gambit/threads)
(import :std/srfi/43) ; vector

(SDL_Init SDL_INIT_VIDEO)

(load "life_shared.scm")

(define (upto-generator n)
  (lambda ()
    (letrec ((do (lambda (current end)
                   (when (< current end)
                     (yield current)
                     (do (1+ current) end)))))
      (do 0 n))))

;;;; setup
(define window-width 1024)
(define window-height 768)
(define block-width 24)
(define block-height 24)

(define arena-width (fx/ window-width block-width))
(define arena-height (fx/ window-height block-height))

(define (draw-block x y surface)
  (SDL_FillRect surface
                (make-temp-rect x y block-width block-height)
                (SDL_MapRGB (SDL_Surface#format surface) 255 0 0)))
  
(define (draw-empty x y surface)
  (SDL_FillRect surface
                (make-temp-rect x y block-width block-height)
                (SDL_MapRGB (SDL_Surface#format surface) 0 0 0)))

(define (make-arena)
  (make-vector (fx* arena-width arena-height) #f))

(define (arena-idx* x y)
  (let ((xx (if (fx< x 0)
              (fx+ x arena-width)
              x))
        (yy (if (fx< y 0)
              (fx+ y arena-height)
              y)))
    (fx+ (fx* (fxmodulo yy arena-height) arena-width)
         (fxmodulo xx arena-width))))

(define (arena-ref arena x y)
  (vector-ref arena (arena-idx* x y)))

(define (arena-set! arena x y v)
  (vector-set! arena (arena-idx* x y) v))

(define (arena-randomize! arena)
  (for (y (upto-generator arena-height))
    (for (x (upto-generator arena-width))
      (let* ((random-value (random-integer 2))
             (value (= 0 (modulo random-value 2))))
        (arena-set! arena x y value)))))

(define (arena-render arena surface)
  (for (y (upto-generator arena-height))
    (for (x (upto-generator arena-width))
      (if (arena-ref arena x y)
        (draw-block (fx* block-width x) (fx* block-height y) surface)
        (draw-empty (fx* block-width x) (fx* block-height y) surface)))))

(define (arena-display arena)
  (for (y (upto-generator arena-height))
    (for (x (upto-generator arena-width))
      (if (arena-ref arena x y)
        (display "X")
        (display ".")))
    (newline)))

(define arena (make-arena))

;;;; Conway's Game of Life
;;;;
;;;; Events happen simultaneously
;;;; 1. Alive cells with fewer than 2 neighbours die.
;;;; 2. Alive cells with 2-3 neighbours lives.
;;;; 3. Alive cells with greater than 3 neighbours die.
;;;; 4. Dead cells with exacly three live neighbours comes to life.
(define (life-tick-state alive neighbours)
  (cond
   ((and alive (< neighbours 2)) #f)
   ((and alive (> neighbours 3)) #f)
   ((and (not alive) (= neighbours 3)) #t)
   (else alive)))

(define (life-tick-inner arena x y)
  (let* ((alive (arena-ref arena x y))
         (neighbours (arena-surrounds-alive arena x y)))
    (life-tick-state alive neighbours)))

(define (life-tick arena)
  (let ((new-arena (make-arena)))
    (for (y (upto-generator arena-height))
      (for (x (upto-generator arena-width))
        (arena-set! new-arena x y (life-tick-inner arena x y))))        
    new-arena))

(define (arena-surrounds-alive arena x y)
  (let ((alive 0))
    (for (yy (upto-generator 3))
      (for (xx (upto-generator 3))
        (let* ((get-x (1- (fx+ x xx)))
               (get-y (1- (fx+ y yy)))
               (self (and (= get-x x) (= get-y y)))
               (alivep (arena-ref arena get-x get-y)))
          (when (and alivep (not self))
            (set! alive (1+ alive))))))
    alive))

;; return a 3x3 vector of surrounds
(define (arena-surrounds arena x y)
  (define (doit result)
    (for (yy (upto-generator 3))
      (for (xx (upto-generator 3))
        (let ((get-x (1- (fx+ x xx)))
              (get-y (1- (fx+ y yy))))
            (vector-set! result (fx+ xx (fx* yy 3))
                         (arena-ref arena get-x get-y)))))
    result)
  (let ((result (make-vector (fx* 3 3) #f)))
    (doit result)))

(define (arena-surrounds-display arena x y)
  (let ((surrounds (arena-surrounds arena x y)))
    (for (y (upto-generator 3))
      (for (x (upto-generator 3))
        (if (vector-ref surrounds (fx+ x (fx* y 3)))
          (display "X")
          (display ".")))
      (newline))))

(arena-randomize! arena)

;;;; main loop
(let* ((window (SDL_CreateWindow "Game of Life" 0 0 window-width window-height 0))
       (surface (SDL_GetWindowSurface window))
       (running #t)
       (current-time (SDL_GetTicks))
       (frame-limiter (make-frame-limiter 60 current-time))
       (frame-counter (make-frame-counter current-time))
       (event (make-SDL_Event))
       (rect (make-SDL_Rect))
       (frame-rate 0)
       (pause #f)
       (life-interval (make-interval 100 current-time
                                          (lambda ()
                                            (unless pause
                                              (set! arena (life-tick arena))))))
       (redisplay-interval (make-interval 100 current-time
                                          (lambda ()
                                            (arena-render arena surface)
                                            (SDL_UpdateWindowSurface window)))))

  (while running
    (let* ((current-time (SDL_GetTicks))
           (delay-time (frame-limiter current-time)))
      (SDL_Delay delay-time)

      ;; only display FPS on rate change
      (let ((new-frame-rate (frame-counter current-time)))
        (when (not (fx= frame-rate new-frame-rate))
          (displayln (format "fps: ~a" new-frame-rate))
          (set! frame-rate new-frame-rate)))

      ;; run simulation and redraw
      (life-interval current-time)
      (redisplay-interval current-time)

      (let event-loop ()
        (let* ((e (SDL_PollEvent event))
               (event-type (SDL_Event#type event)))
          (when (fx> e 0)
            (cond
             ((fx= event-type SDL_MOUSEBUTTONDOWN)
              (let* ((button-event (SDL_Event#button-ref event))
                     (button (SDL_MouseButtonEvent#button button-event))
                     (x (SDL_MouseButtonEvent#x button-event))
                     (y (SDL_MouseButtonEvent#y button-event))
                     (block-x (fx/ x block-width))
                     (block-y (fx/ y block-height)))
                (arena-set! arena block-x block-y
                            (not (arena-ref arena block-x block-y)))))
             ((fx= event-type SDL_KEYDOWN)
              (let* ((keyboard-event (SDL_Event#key-ref event))
                     (keysym (SDL_KeyboardEvent#keysym-ref keyboard-event))
                     (key-code (SDL_Keysym#sym keysym)))
                (cond
                 ((fx= key-code SDLK_RETURN)
                  (arena-randomize! arena))
                 ((fx= key-code SDLK_SPACE)
                  (set! pause (not pause))
                  (if pause
                    (displayln "Paused. Press space to unpause.")
                    (displayln "Unpaused. Press space to pause.")))
                 ((fx= key-code SDLK_ESCAPE)
                  (displayln "Escape key pressed. Exiting")
                  (set! running #f))
                 (else
                  (displayln (format "Unhandled key: ~a"
                                     (if (< key-code 256)
                                       (integer->char key-code)
                                       key-code)))))))
             ((fx= event-type SDL_QUIT) (set! running #f)))
            (event-loop)))))))

      

(SDL_Quit)
(##gc)

(displayln "Goodbye :(")
