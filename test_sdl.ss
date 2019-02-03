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
        (min (fx/ 1000 since)
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

(define color-white
  (let ((color (make-SDL_Color)))
    (SDL_Color#r! color 255)
    (SDL_Color#g! color 255)
    (SDL_Color#b! color 255)
    color))

(SDL_Init SDL_INIT_EVERYTHING)
(TTF_Init)
;; (define ttf-font (TTF_OpenFont "iosevka-bold.ttc" 48))
;; (define hello-surface (TTF_RenderText_Solid ttf-font "Hello world!" color-white))

(let* ((window (SDL_CreateWindow "Hello SDL" 0 0 800 600 0))
       (bmp (IMG_Load "nyan.png"))
       (surface (SDL_GetWindowSurface window))
       (running #t)
       (current-time (SDL_GetTicks))
       (frame-limiter (make-frame-limiter 30 current-time))
       (frame-counter (make-frame-counter current-time))
       (repl-server #f)
       (event (make-SDL_Event)))

  (displayln (format "Loaded: ~a~%" bmp))

  ;; ???
  ;; (spawn
  ;;  (lambda ()
  ;;    (set! repl-server (start-repl-server! password: "password"
  ;;                                          address: "localhost:54321"))))

  (define frame-rate 0)

  (while running
    (SDL_UpdateWindowSurface window)

    (let* ((current-time (SDL_GetTicks))
           (delay-time (frame-limiter current-time)))
      (SDL_Delay delay-time)

      ;; only display FPS on rate change
      (let ((new-frame-rate (frame-counter current-time)))
        (when (not (= frame-rate new-frame-rate))
          (displayln (format "fps: ~a" (frame-counter current-time))))
        (set! frame-rate new-frame-rate))

      (let event-loop ()
        (let ((e (SDL_PollEvent event))
              (event-type (SDL_Event#type event)))
          (when (> e 0)
            ;; process events
            ;; (displayln (format "event: ~a" event))
            (cond
             ((= event-type SDL_KEYDOWN)
              (let* ((keyboard-event (SDL_Event#key event))
                     (keysym (SDL_KeyboardEvent#keysym
                              (SDL_KeyboardEvent->SDL_KeyboardEvent* keyboard-event)))
                     (key-code (SDL_Keysym#sym (SDL_Keysym->SDL_Keysym* keysym))))
                (cond
                 ((= key-code SDLK_ESCAPE)
                  (displayln "Escape key pressed. Exiting")
                  (set! running #f))
                 (else
                  (displayln (format "Unhandled key: ~a" key-code))))))
             ((= event-type SDL_QUIT) (set! running #f)))
            (event-loop))))

      ;; redisplay
      (SDL_BlitSurface bmp #f surface #f)))
      ;; (SDL_BlitSurface hello-surface #f surface #f)

  ;; (stop-repl-server! repl-server)
  (SDL_DestroyWindow window))
(SDL_Quit)

(displayln "Goodbye :(")
