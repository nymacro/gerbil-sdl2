;; simple test application
(import :sdl2/sdl2)

(import :std/format)
(import :std/sugar)
(import :gerbil/gambit/threads)

(define ttf-font-path "/usr/local/share/fonts/hack-font/Hack-Bold.ttf")

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
  (let ((last-time initial-time)
        (fps 0)
        (counter 0))
    (lambda (current-time)
      (set! counter (fx1+ counter))
      (when (fx> current-time (fx+ last-time 1000))
        (set! fps counter)
        (set! counter 0)
        (set! last-time current-time))
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

(SDL_Init SDL_INIT_VIDEO)
(TTF_Init)

(define ttf-font (TTF_OpenFont ttf-font-path 48))
(define hello-surface (TTF_RenderText_Blended ttf-font "Hello!" color-cyan))

(define (make-marquee from to span-time init-time)
  (let* ((last-time init-time)
         (distance (fx- to from)))
    (lambda (current-time)
      (let* ((elapsed (fx- current-time last-time))
             (new-current (if (fx= elapsed 0)
                            from
                            (+ from (* distance (/ elapsed span-time))))))
        (when (> elapsed span-time)
          (set! last-time (fx- current-time (fx- span-time elapsed))))
        (##flonum->fixnum (exact->inexact new-current))))))

(define (make-circular fn distance span-time init-time)
  (let* ((last-time init-time)
         (angle 0)
         (pi 3.14159)
         (fix (lambda (i)
                (##flonum->fixnum (exact->inexact i)))))
    (lambda (current-time)
      (let* ((elapsed (fx- current-time last-time))
             (ratio (/ elapsed span-time))
             (new-angle (* 2 pi ratio))
             (nx (* (fx/ distance 2) (fn new-angle))))
        (when (> new-angle (* 2 pi))
          (set! last-time current-time))
        (fix nx)))))

(define (make-sine distance span-time init-time)
  (make-circular sin distance span-time init-time))

(define (make-cosine distance span-time init-time)
  (make-circular cos distance span-time init-time))

(define (make-orbit distance span-time init-time)
  (let ((s (make-sine distance span-time init-time))
        (c (make-cosine distance span-time init-time)))
    (lambda (current-time)
      (cons (c current-time) (s current-time)))))

(let* ((window (SDL_CreateWindow "Hello SDL" 0 0 800 600 0))
       (surface (SDL_GetWindowSurface window))
       (running #t)
       (current-time (SDL_GetTicks))
       (frame-limiter (make-frame-limiter 60 current-time))
       (frame-counter (make-frame-counter current-time))
       (event (make-SDL_Event))
       (orbit (make-orbit 200 2000 current-time))
       (waver (make-sine 400 1250 current-time))
       (rect (make-SDL_Rect))
       (frame-rate 0))

  (while running
    (let* ((current-time (SDL_GetTicks))
           (delay-time (frame-limiter current-time)))
      (SDL_Delay delay-time)

      ;; only display FPS on rate change
      (let ((new-frame-rate (frame-counter current-time)))
        (when (not (fx= frame-rate new-frame-rate))
          (displayln (format "fps: ~a" new-frame-rate))
          (set! frame-rate new-frame-rate)))

      (let event-loop ()
        (let ((e (SDL_PollEvent event))
              (event-type (SDL_Event#type event))
              ;; remove displayln binding for event logging
              (displayln void))
          (when (fx> e 0)
            (cond
             ((fx= event-type SDL_MOUSEBUTTONDOWN)
              (let* ((button-event (SDL_Event#button-ref event))
                     (button (SDL_MouseButtonEvent#button button-event)))
                (displayln (format "Mouse press: ~a" button))))
             ((fx= event-type SDL_MOUSEMOTION)
              (let* ((motion-event (SDL_Event#motion-ref event))
                     (x (SDL_MouseMotionEvent#x motion-event))
                     (y (SDL_MouseMotionEvent#y motion-event)))
                     ;; (rx (SDL_MouseMotionEvent#xrel motion-event))
                     ;; (ry (SDL_MouseMotionEvent#yrel motion-event)))
                (displayln (format "x: ~a, y: ~a" x y))))
             ((fx= event-type SDL_KEYDOWN)
              (let* ((keyboard-event (SDL_Event#key-ref event))
                     (keysym (SDL_KeyboardEvent#keysym-ref keyboard-event))
                     (key-code (SDL_Keysym#sym keysym)))
                (cond
                 ((fx= key-code SDLK_ESCAPE)
                  (displayln "Escape key pressed. Exiting")
                  (set! running #f))
                 (else
                  (displayln (format "Unhandled key: ~a"
                                     (if (< key-code 256)
                                       (integer->char key-code)
                                       key-code)))))))
             ((fx= event-type SDL_QUIT) (set! running #f)))
            (event-loop))))

      ;; redisplay
      (SDL_FillRect surface #f (SDL_MapRGB (SDL_Surface#format surface) 0 0 0))

      (let ((pair (orbit current-time))
            (x-offset (fx- (fx/ (SDL_Surface#w surface) 2)
                           (fx/ (SDL_Surface#w hello-surface) 2)))
            (y-offset (fx- (fx/ (SDL_Surface#h surface) 2)
                           (fx/ (SDL_Surface#h hello-surface) 2))))
        (SDL_Rect#x! rect (fx+ x-offset
                               (waver current-time)
                               (car pair)))
        (SDL_Rect#y! rect (fx+ y-offset
                               (cdr pair))))
      (SDL_BlitSurface hello-surface #f surface rect)

      ;; flip surface
      (SDL_UpdateWindowSurface window)))

  (SDL_DestroyWindow window))

(SDL_Quit)

(displayln "Goodbye :(")
