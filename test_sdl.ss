(import :sdl2/sdl2)

(import :std/format)
(import :std/sugar)
(import :std/net/repl)
(import :gerbil/gambit/threads)

(define (make-frame-limiter frame-max initial-time)
  (let ((last-time initial-time)
        (max-delay (fx/ 1000 frame-max)))
    (lambda (current-time)
      (let ((since (fx- current-time last-time)))
        (set! last-time current-time)
        (min (fx/ 1000 (max 1 since))
             max-delay)))))

(define (make-frame-counter initial-time)
  (let ((last-time initial-time)
        (fps 0)
        (counter 0))
    (lambda (current-time)
      (set! counter (fx1+ counter))
      (when (fx> current-time (fx+ last-time 1000))
        (set! fps (fx/ (fx+ counter fps) 2))
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

(SDL_Init SDL_INIT_EVERYTHING)
(TTF_Init)
(define ttf-font (TTF_OpenFont "iosevka-bold.ttc" 48))
(define hello-surface (TTF_RenderText_Blended ttf-font "Hello world!" color-cyan))

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
       (repl-server #f)
       (event (make-SDL_Event))
       (marquee-x (make-marquee 0 800 3000 current-time))
       (marquee-y (make-marquee 0 600 3000 current-time))
       (orbit (make-orbit 200 2000 current-time))
       (waver (make-sine 400 1250 current-time))
       (rect (make-SDL_Rect)))

  (define frame-rate 0)

  (while running
    (SDL_UpdateWindowSurface window)

    (let* ((current-time (SDL_GetTicks))
           (delay-time (frame-limiter current-time)))
      (SDL_Delay delay-time)

      ;; only display FPS on rate change
      (let ((new-frame-rate (frame-counter current-time)))
        (when (not (fx= frame-rate new-frame-rate))
          (displayln (format "fps: ~a" (frame-counter current-time))))
        (set! frame-rate new-frame-rate))

      (let event-loop ()
        (let ((e (SDL_PollEvent event))
              (event-type (SDL_Event#type event)))
          (when (fx> e 0)
            ;; process events
            ;; (displayln (format "event: ~a" event))
            (cond
             ((fx= event-type SDL_KEYDOWN)
              (let* ((keyboard-event (SDL_Event#key event))
                     (keysym (SDL_KeyboardEvent#keysym
                              (SDL_KeyboardEvent->SDL_KeyboardEvent* keyboard-event)))
                     (key-code (SDL_Keysym#sym (SDL_Keysym->SDL_Keysym* keysym))))
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
      (let ((x (marquee-x current-time))
            (y (marquee-y current-time)))
        (SDL_Rect#x! rect x)
        (SDL_Rect#y! rect y))
      (SDL_BlitSurface hello-surface #f surface rect)

      (let ((pair (orbit current-time)))
        (SDL_Rect#x! rect (fx+ 200 (car pair)))
        (SDL_Rect#y! rect (fx+ 200 (cdr pair))))
      (SDL_BlitSurface hello-surface #f surface rect)

      (let ((x (waver current-time)))
        (SDL_Rect#x! rect (fx+ 200 x))
        (SDL_Rect#y! rect 0))
      (SDL_BlitSurface hello-surface #f surface rect)))

  ;; (stop-repl-server! repl-server)
  (SDL_DestroyWindow window))
(SDL_Quit)

(displayln "Goodbye :(")
