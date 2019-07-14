;;;; gambit-sdl2 tetris

(declare
  (r5rs-scheme)
  (block)
  (standard-bindings)
  (extended-bindings)
  (not run-time-bindings))

(##include "~~lib/_syntax.scm")

(load "sdl2")
(load "sdl2-ttf")

(SDL_Init SDL_INIT_VIDEO)
(TTF_Init)

;;;; helper functions
(define (make-interval interval init-time action)
  (let ((last-time init-time))
    (lambda (current-time)
      (when (fx> current-time (fx+ last-time interval))
        (action)
        (set! last-time (fx+ last-time interval))))))

(define (1+ x) (+ x 1))
(define (fx1+ x) (1+ x))
(define (1- x) (- x 1))
(define (fx1- x) (1- x))

(define displayln
  (case-lambda
    ((str port)
     (display str port)(newline port))
    ((str)
     (display str)(newline))))

(define-syntax for
  (syntax-rules ()
    ((_ (var init end) stmt ...)
     (let ((var init))
       (let loop ()
         (when (< var end)
           (begin
             stmt ...)
           (set! var (1+ var))
           (loop)))))))

(define fx/ fxquotient)

;; TODO fix precision of limiting
;; (define (make-frame-limiter frame-max initial-time)
;;   (let ((last-time initial-time)
;;         (max-delay (fx/ 1000 frame-max)))
;;     (lambda (current-time)
;;       (let* ((since (fx- current-time last-time))
;;              (delay-time (if (fx<= since 1)
;;                            0
;;                            (fx/ 1000 since))))
;;         (set! last-time current-time)
;;         (min delay-time max-delay)))))
(define (make-frame-limiter frame-max initial-time)
  (let ((last-time initial-time)
        (max-delay (fx/ 1000 frame-max)))
    (lambda (current-time)
      (let* ((since (fx- current-time last-time))
             (delay-time (if (fx<= since 1)
                           0
                           (fx/ 1000 since))))
        (set! last-time current-time)
        (min delay-time max-delay)))))

(define (make-frame-counter initial-time)
  (let ((next-time (fx+ initial-time 1000))
        (fps 0)
        (counter 0))
    (lambda (current-time)
      (set! counter (fx1+ counter))
      (when (fx>= current-time next-time)
        (set! fps counter)
        (set! counter 0)
        (set! next-time (fx+ next-time 1000)))
      fps)))

(define-syntax literal-color
  (syntax-rules ()
    ((_ r g b)
     (let ((color (make-SDL_Color)))
       (SDL_Color#r! color r)
       (SDL_Color#g! color g)
       (SDL_Color#b! color b)
       (SDL_Color*->SDL_Color color)))))

(define color-white (literal-color 255 255 255))
(define color-cyan (literal-color 255 0 255))
(define color-black (literal-color 0 0 0))

(define-syntax make-rect
  (syntax-rules ()
    ((_ x y w h)
     (let ((rect (make-SDL_Rect)))
       (SDL_Rect#x! rect x)
       (SDL_Rect#y! rect y)
       (SDL_Rect#w! rect w)
       (SDL_Rect#h! rect h)
       rect))))

(define make-temp-rect
  (let ((rect (make-rect 0 0 0 0)))
    (lambda (x y w h)
      (SDL_Rect#x! rect x)
      (SDL_Rect#y! rect y)
      (SDL_Rect#w! rect w)
      (SDL_Rect#h! rect h)
      rect)))

;;;; setup
(define window-width 1024)
(define window-height 768)
(define block-width 24)
(define block-height 24)

(define (draw-block x y surface)
  (SDL_FillRect surface
                (make-temp-rect x y block-width block-height)
                (SDL_MapRGB (SDL_Surface#format surface) 255 0 0)))
  
(define (draw-empty x y surface)
  (SDL_FillRect surface
                (make-temp-rect x y block-width block-height)
                (SDL_MapRGB (SDL_Surface#format surface) 0 0 0)))

(define arena-width (fx/ window-width block-width))
(define arena-height (fx/ window-height block-height))

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

(define-syntax for-arena
  (syntax-rules ()
    ((for-arena (x y) stmt stmts ...)
     (for (y 0 arena-height)
       (for (x 0 arena-width)
         stmt
         stmts ...)))))

(define (arena-ref arena x y)
  (vector-ref arena (arena-idx* x y)))

(define (arena-set! arena x y v)
  (vector-set! arena (arena-idx* x y) v))

(define (arena-randomize! arena)
  (for-arena (x y) 
    (let* ((random-value (random-integer 2))
           (value (= 0 (modulo random-value 2))))
      (arena-set! arena x y value))))

(define (arena-clear! arena)
  (for-arena (x y)
    (arena-set! arena x y #f)))

(define (arena-render arena surface)
  (for-arena (x y)
    (if (arena-ref arena x y)
      (draw-block (fx* block-width x) (fx* block-height y) surface)
      (draw-empty (fx* block-width x) (fx* block-height y) surface))))

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
  (cond
   ((and alive (< neighbours 2)) #f)
   ((and alive (> neighbours 3)) #f)
   ((and (not alive) (= neighbours 3)) #t)
   (else alive)))

(define (vector-fold fn init vec)
  (define (vector-iterate vec fn)
    (let ([len (vector-length vec)])
      (define (xdo idx)
        (when (< idx len)
          (fn idx (vector-ref vec idx))
          (xdo (1+ idx))))
      (xdo 0)))
  (let ([result init])
    (vector-iterate vec
                    (lambda (idx v)
                      (set! result (fn idx result v))))
    result))

(define (life-tick-inner arena x y)
  (let* ((alive (arena-ref arena x y))
         (neighbours (arena-surrounds-alive arena x y)))
    (life-tick-state alive neighbours)))

(define (life-tick arena)
  (let ((new-arena (make-arena)))
    (for-arena (x y)
      (arena-set! new-arena x y (life-tick-inner arena x y)))        
    new-arena))

(define (arena-surrounds-alive arena x y)
  (let ((alive 0))
    (for (yy 0 3)
      (for (xx 0 3)
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
    (for (yy 0 3)
      (for (xx 0 3)
        (let ((get-x (1- (fx+ x xx)))
              (get-y (1- (fx+ y yy))))
            (vector-set! result (fx+ xx (fx* yy 3))
                         (arena-ref arena get-x get-y)))))
    result)
  (let ((result (make-vector (fx* 3 3) #f)))
    (doit result)))

(define (arena-surrounds-display arena x y)
  (let ((surrounds (arena-surrounds arena x y)))
    (for (y 0 3)
      (for (x 0 3)
        (if (vector-ref surrounds (fx+ x (fx* y 3)))
          (display "X")
          (display ".")))
      (newline))))

(define arena (make-arena))
(arena-randomize! arena)

(define ttf-font-path "/usr/local/share/fonts/hack-font/Hack-Bold.ttf")
(define ttf-font (TTF_OpenFont ttf-font-path 48))
(define hello-surface (TTF_RenderText_Blended ttf-font "Hello!" color-cyan))


(let* ((window (SDL_CreateWindow "Game of Life" 0 0 window-width window-height 0))
       (surface (SDL_GetWindowSurface window))
       (event (make-SDL_Event))
       (running #t)
       (current-time (SDL_GetTicks))
       (frame-limiter (make-frame-limiter 60 current-time))
       (frame-counter (make-frame-counter current-time))
       (frame-rate 0)
       (pause #f)
       (life-interval (make-interval 100 current-time
                                          (lambda ()
                                            (unless pause
                                              (set! arena (life-tick arena))))))
       (redisplay-interval (make-interval 100 current-time
                                          (lambda ()
                                            (arena-render arena surface)
                                            (SDL_BlitSurface hello-surface #f surface (make-temp-rect 0 0 0 0))
                                            (SDL_UpdateWindowSurface window)))))

  ;;;; main loop
  (let loop ()
    (let* ((current-time (SDL_GetTicks))
           (delay-time (frame-limiter current-time)))
      (SDL_Delay delay-time)

      ;; only display FPS on rate change
      (let ((new-frame-rate (frame-counter current-time)))
        (when (not (fx= frame-rate new-frame-rate))
          (displayln (string-append "fps: " (object->string new-frame-rate)))
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
                  (displayln (string-append "Unhandled key:"
                                            (object->string (if (< key-code 256)
                                                              (integer->char key-code)
                                                              key-code))))))))
             ((fx= event-type SDL_QUIT) (set! running #f)))
            (event-loop)))))

    (when running (loop))))

(SDL_Quit)
(##gc)

(displayln "Goodbye :(")
