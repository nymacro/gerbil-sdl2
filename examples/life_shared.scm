;;;; Copyright (C) 2019 Aaron Marks. All Rights Reserved.

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


;;; helper functions
(define (make-interval interval init-time action)
  (let ((last-time init-time))
    (lambda (current-time)
      (when (fx> current-time (fx+ last-time interval))
        (action)
        (set! last-time (fx+ last-time interval))))))

;; TODO fix precision of limiting
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

