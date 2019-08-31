;;;; gambit-sdl2 Game of Life
;;;; Copyright (C) 2019 Aaron Marks. All Rights Reserved.

(declare
  (r5rs-scheme)
  (block)
  (standard-bindings)
  (extended-bindings)
  (fixnum)
  (run-time-bindings)
  (not safe))

(##include "~~lib/_syntax.scm")

(load "sdl2")

(##include "life_shared.scm")

(SDL_Init SDL_INIT_VIDEO)

;;;; helper functions
(define (1+ x) (+ x 1))
(define (fx1+ x) (fx+ x 1))
(define (1- x) (- x 1))
(define (fx1- x) (fx- x 1))
(define fx/ fxquotient)

(define displayln
  (case-lambda
    ((str port)
     (display str port)(newline port))
    ((str)
     (display str)(newline))))

(define-syntax with-mutex
  (syntax-rules ()
    ((_ mtx x xs ...)
     (begin
       (dynamic-wind
         (lambda () (mutex-lock! mtx))
         (lambda ()
           x
           xs ...)
         (lambda () (mutex-unlock! mtx)))))))


;;;; setup
(define window-width 800)
(define window-height 600)
(define block-width 8)
(define block-height 8)

(define arena-width (fx/ window-width block-width))
(define arena-height (fx/ window-height block-height))

(define (draw-block x y renderer #!optional (intensity 0))
  (SDL_SetRenderDrawColor renderer intensity 0 0 255)
  (SDL_RenderFillRect renderer (make-temp-rect x y block-width block-height)))

(define (make-arena)
  (make-u8vector (fx* arena-width arena-height) 0))

(define (arena-idx* x y)
  (let ((xx (if (fx< x 0)
              (fx+ x arena-width)
              x))
        (yy (if (fx< y 0)
              (fx+ y arena-height)
              y)))
    (fx+ (fx* (fxmodulo yy arena-height) arena-width)
         (fxmodulo xx arena-width))))

(define-syntax life-alive-p
  (syntax-rules ()
    ((_ x)
     (fx= x 255))))
(define-syntax life-drain
  (syntax-rules ()
    ((_ x)
     (cond
      ((fx> x 0) (max 0 (fx- x 16)))
      (else 0)))))

(define-syntax for-arena
  (syntax-rules ()
    ((for-arena (x y) stmt stmts ...)
     (for (y 0 arena-height)
       (for (x 0 arena-width)
         stmt
         stmts ...)))))

(define (arena-ref arena x y)
  (u8vector-ref arena (arena-idx* x y)))

(define (arena-set! arena x y v)
  (u8vector-set! arena (arena-idx* x y) v))

(define (arena-randomize! arena)
  (for-arena (x y) 
    (let* ((random-value (random-integer 2))
           (value (if (fx= 0 (modulo random-value 2))
                    0
                    255)))
      (arena-set! arena x y value))))

(define (arena-clear! arena)
  (for-arena (x y)
    (arena-set! arena x y 0)))

(define (arena-render arena renderer)
  (for-arena (x y)
    (let ((alive (arena-ref arena x y)))
      (draw-block (fx* block-width x) (fx* block-height y) renderer alive))))

(define (arena-display arena)
  (for (y 0 arena-height)
    (for (x 0 arena-width)
      (if (arena-ref arena x y)
        (display "X")
        (display ".")))
    (newline)))

;;;; Conway's Game of Life
;;;;
;;;; Events happen simultaneously
;;;; 1. Alive cells with fewer than 2 neighbours die.
;;;; 2. Alive cells with 2-3 neighbours lives.
;;;; 3. Alive cells with greater than 3 neighbours die.
;;;; 4. Dead cells with exacly three live neighbours comes to life.
(define (life-tick-state alive neighbours)
  (let ((alivep (life-alive-p alive)))
    (cond
     ((and alivep (< neighbours 2)) (life-drain alive))
     ((and alivep (> neighbours 3)) (life-drain alive))
     ((and (not alivep) (= neighbours 3)) 255)
     (else
      (if (life-alive-p alive)
        alive
        (life-drain alive))))))

(define (life-tick-inner arena x y)
  (let* ((alive (arena-ref arena x y))
         (neighbours (arena-surrounds-alive arena x y)))
    (life-tick-state alive neighbours)))

(define (life-tick arena)
  (let ((new-arena (make-arena)))
    (for-arena (x y)
      (arena-set! new-arena x y (life-tick-inner arena x y)))        
    new-arena))

(define (life-tick-pause arena)
  (let ((new-arena (make-arena)))
    (for-arena (x y)
      (let ((alive (arena-ref arena x y)))
        (arena-set! new-arena x y (if (life-alive-p alive)
                                    alive
                                    (life-drain alive)))))
    new-arena))

;; return number of alive neighbours for a cell
(define (arena-surrounds-alive arena x y)
  (let ((alive 0))
    (for (yy 0 3)
      (for (xx 0 3)
        (let* ((get-x (1- (fx+ x xx)))
               (get-y (1- (fx+ y yy)))
               (self (and (= get-x x) (= get-y y)))
               (alivep (life-alive-p (arena-ref arena get-x get-y))))
          (when (and alivep (not self))
            (set! alive (1+ alive))))))
    alive))

(define arena (make-arena))
(arena-randomize! arena)

(let* ((window (SDL_CreateWindow "Game of Life" 0 0 window-width window-height 0))
       (fullscreen #f)
       (toggle-fullscreen (lambda ()
                            (set! fullscreen (not fullscreen))
                            (SDL_SetWindowFullscreen window (if fullscreen SDL_TRUE SDL_FALSE))))
       (renderer (SDL_CreateRenderer window -1 SDL_RENDERER_SOFTWARE))
       (event (make-SDL_Event))
       (running #t)
       (current-time (SDL_GetTicks))
       (frame-limiter (make-frame-limiter 30 current-time))
       (frame-counter (make-frame-counter current-time))
       (frame-rate 0)
       (pause #f)
       (mtx (make-mutex))
       (life-action (lambda ()
                      (let* ((current-time (SDL_GetTicks))
                             (life-counter (make-frame-counter current-time))
                             (life-limiter (make-frame-limiter 60 current-time))
                             (last-rate 0))
                        (let loop ()
                          (let* ((current-time (SDL_GetTicks))
                                 (count (life-counter current-time))
                                 (delay-time (life-limiter current-time)))
                            (when (not (fx= last-rate count))
                              (displayln (string-append "life: " (object->string count)))
                              (set! last-rate count))
                            (thread-sleep! (/ delay-time 1000))
                            (with-mutex mtx
                              (if pause
                                (set! arena (life-tick-pause arena))
                                (set! arena (life-tick arena)))))
                          (loop)))))
       (life-thread (thread-start! (make-thread life-action "life-tick"))))

  ;; main loop
  (let loop ()
    (let* ((current-time (SDL_GetTicks))
           (delay-time (frame-limiter current-time))
           (new-frame-rate (frame-counter current-time)))
      (when (not (fx= frame-rate new-frame-rate))
        (displayln (string-append "fps: " (object->string new-frame-rate)))
        (set! frame-rate new-frame-rate))
      (thread-sleep! (/ delay-time 1000))

      ;; redraw
      (with-mutex mtx
        (arena-render arena renderer)
        (SDL_RenderPresent renderer))

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
                (with-mutex mtx
                  (arena-set! arena block-x block-y
                              (if (life-alive-p (arena-ref arena block-x block-y))
                                0
                                255)))))
             ((fx= event-type SDL_KEYDOWN)
              (let* ((keyboard-event (SDL_Event#key-ref event))
                     (keysym (SDL_KeyboardEvent#keysym-ref keyboard-event))
                     (key-code (SDL_Keysym#sym keysym)))
                (cond
                 ((fx= key-code SDLK_RETURN)
                  (with-mutex mtx
                    (arena-randomize! arena)))
                 ((fx= key-code SDLK_SPACE)
                  (set! pause (not pause))
                  (if pause
                    (displayln "Paused. Press space to unpause.")
                    (displayln "Unpaused. Press space to pause.")))
                 ((fx= key-code SDLK_c)
                  (arena-clear! arena))
                 ((fx= key-code SDLK_r)
                  ;; launch REPL on C-r
                  (unless (zero? (bitwise-and (SDL_Keysym#mod keysym) KMOD_CTRL))
                    (when fullscreen toggle-fullscreen)
                    (##continuation-capture
                     (lambda (cont)
                       (##repl-within cont #f #f)))))
                 ((fx= key-code SDLK_ESCAPE)
                  (displayln "Escape key pressed. Exiting")
                  (set! running #f))
                 ((fx= key-code SDLK_f)
                  (toggle-fullscreen))
                 (else
                  (displayln (string-append "Unhandled key:"
                                            (object->string (if (< key-code 256)
                                                              (integer->char key-code)
                                                              key-code))))))))
                 
             ((fx= event-type SDL_QUIT) (set! running #f)))
            (event-loop)))))

    (when running (loop)))
  (thread-terminate! life-thread))

(##gc)
(SDL_Quit)

(displayln "Goodbye :(")
