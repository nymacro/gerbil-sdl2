;;; Copyright (c) 2013-2014 by Álvaro Castro Castilla. All Rights Reserved.
;;; Copyright (c) 2019 Aaron Marks. All Rights Reserved.
;;; SDL2 Foreign Function Interface

(define sdl2-ref-table (make-table weak-keys: #t))

(define (sdl2-tie to from)
  (println (string-append "tie " (object->string to) " to " (object->string from)))
  (table-set! sdl2-ref-table to from))

(define (sdl2-untie to)
  (println (string-append "untie " (object->string to)))
  (table-set! sdl2-ref-table to (table-ref sdl2-ref-table to)))

(define (make-sdl2-tied tie-to-arg create)
  (lambda args
    (let ((c (apply create args)))
      (sdl2-tie c (list-ref args tie-to-arg))
      c)))

(define (make-sdl2-untie final)
  (lambda (p)
    (sdl2-untie p)
    (final p)))

;; helper to attach a finalizer to a Scheme object
(define-macro (finalize-with finalizer value)
  `(begin
     (make-will ,value (lambda (x)
                         (println (string-append "freeing " (object->string x)
                                                 " with " (object->string ,finalizer)))
                         (,finalizer x)))
     ,value))

;; wrapper function for adding finalizer
(define (c-final final l)
  (lambda args
    (let ((retval (apply l args)))
      (finalize-with final retval)
      retval)))

;; wrapped function for adding return value checking
(define (c-checked check l)
  (lambda args
    (let ((retval (apply l args)))
      (unless (check retval)
        (println (string-append "Unexpected return value from " l
                                ": " (object->string retval)))
        (abort retval))
      retval)))

(define (true? p)
  (eq? #t p))
(define (false? p)
  (eq? #f p))
(define (!false? p)
  (not (false? p)))

(define-macro (c-define-constants . consts)
  (let ((to-c-lambda (lambda (const)
                       `(begin
                          (define ,const
                            ((c-lambda () unsigned-int
                               ,(string-append "___result = "
                                               (symbol->string const)
                                               ";"))))))))
    `(begin ,@(map to-c-lambda consts))))

;; FIXME
(define-macro (c-define-enum name . consts)
  (let ((to-sym (string->symbol (string-append (symbol->string name) "#to-enum")))
        (from-sym (string->symbol (string-append (symbol->string name) "#from-enum")))
        (table-name (gensym name))
        (rev-table-name (gensym name)))
    `(begin
       (c-define-constants ,@consts)

       ;; create assoc table
       (let ((,table-name (list->table (map (lambda (name)
                                              (cons name (call name)))
                                            ,consts)))
             (,rev-table-name (list->table (map (lambda (name)
                                                  (cons name (call name)))
                                                ,consts))))
         (define (,to-sym from)
           (table-ref ,table-name from))
         (define (,from-sym to)
           (table-ref ,rev-table-name to))))))

(define-macro (c-define-struct name . fields)
  (let* ((name-ptr (string->symbol (string-append (symbol->string name) "*")))
         (name-str (symbol->string name))
         (constructor (string->symbol (string-append "make-" (symbol->string name))))
         (to-c-lambda (lambda (field)
                        (let* ((field-name (symbol->string (car field)))
                               (field-type (cadr field))
                               (getter (string->symbol
                                        (string-append name-str "#" field-name)))
                               (setter (string->symbol
                                        (string-append (symbol->string getter) "!"))))
                          `(begin
                             (define ,getter
                               (c-lambda (,name-ptr) ,field-type
                                 ,(string-append "___return(___arg1->" field-name ");")))
                             (define ,(string->symbol (string-append
                                                       (symbol->string getter)
                                                       "-ref"))
                               (c-lambda (,name-ptr) (pointer ,field-type)
                                 ,(string-append "___return(&___arg1->" field-name ");")))
                             (define ,setter
                               (c-lambda (,name-ptr ,field-type) void ;,field-type
                                 ,(string-append "___arg1->" field-name " = ___arg2;"))))))))
    `(begin
       (define ,constructor
         (let ((c-fn (c-lambda () ,name-ptr
                       ,(string-append "___result = malloc(sizeof(" name-str "));")))
               (f-fn (c-lambda (,name-ptr) void
                       "free(___arg1);")))
           (lambda ()
             (finalize-with f-fn (c-fn)))))

       ;; value/pointer conversions
       (define ,(string->symbol (string-append name-str "*->" name-str))
         (c-lambda (,name-ptr) ,name
           "___return(*(___arg1));"))
       (define ,(string->symbol (string-append name-str "->" name-str "*"))
           (c-lambda (,name) ,name-ptr
             "___return(&___arg1);"))

       ;; insert field functions
       ,@(map to-c-lambda fields))))



(c-declare "#include \"SDL.h\"")
;; Include this file explicitly for the SDL_SysWMinfo declaration
(c-declare "#include \"SDL_syswm.h\"")

;; We need to declare these structs, otherwise we get an
;; incomplete type 'struct SDL_DisplayMode' error
(c-declare #<<end-of-string
  struct SDL_DisplayMode
  {
  Uint32 format;    /**< pixel format */
  int w;            /**< width */
  int h;            /**< height */
  int refresh_rate; /**< refresh rate (or zero for unspecified) */
  void *driverdata;
  };                /**< driver-specific data, initialize to 0 */
end-of-string
)

;; define c-type and pointer
(define-macro (c-define-type* name type)
  `(begin
     (c-define-type ,name ,type)
     (c-define-type ,(string->symbol (string-append (symbol->string name) "*")) (pointer ,name))
     (c-define-type ,(string->symbol (string-append (symbol->string name) "**")) (pointer (pointer ,name)))))

;;------------------------------------------------------------------------------
;;!! Types

(c-define-type void* (pointer void))
(c-define-type void** (pointer void*))
(c-define-type int* (pointer int))
(c-define-type unsigned-int* (pointer unsigned-int))
(c-define-type unsigned-int8* (pointer unsigned-int8))
(c-define-type unsigned-int8** (pointer unsigned-int8*))
(c-define-type unsigned-int16* (pointer unsigned-int16))
(c-define-type unsigned-int32* (pointer unsigned-int32))
(c-define-type unsigned-int64* (pointer unsigned-int64))
(c-define-type float* (pointer float))
(c-define-type size-t size_t)
(c-define-type double* (pointer double))

(c-define-type SDL_AudioDeviceID unsigned-int32)
(c-define-type* SDL_AudioFormat unsigned-int16)
(c-define-type SDL_AudioCallback (function (void* unsigned-int8* int) void))
(c-define-type SDL_AudioStatus int) ; enum
(c-define-type SDL_bool int)
(c-define-type* SDL_BlendMode int) ; enum
(c-define-type* SDL_cond (struct "SDL_cond"))
(c-define-type* SDL_Cursor (struct "SDL_Cursor"))
(c-define-type SDL_eventaction int) ; enum
(c-define-type SDL_FingerID int64)
(c-define-type* SDL_GameController (struct "SDL_GameController"))
(c-define-type SDL_GameControllerAxis int) ; enum
(c-define-type SDL_GameControllerButton int) ; enum
(c-define-type SDL_GameControllerButtonBind (struct "SDL_GameControllerButtonBind"))
(c-define-type SDL_GestureID int64)
(c-define-type SDL_GLContext void*)
(c-define-type SDL_GLattr int) ; enum
(c-define-type* SDL_Haptic (struct "SDL_Haptic"))
(c-define-type SDL_HintPriority int) ; enum
(c-define-type SDL_Joystick (struct "SDL_Joystick"))
(c-define-type SDL_Joystick* (pointer SDL_Joystick))
(c-define-type SDL_JoystickGUID (struct "SDL_JoystickGUID"))
(c-define-type SDL_JoystickID int32)
(c-define-type SDL_Keycode int) ; enum
(c-define-type SDL_Keymod int) ; enum
(c-define-type SDL_LogPriority int) ; enum
(c-define-type SDL_MessageBoxData "SDL_MessageBoxData")
(c-define-type SDL_MessageBoxData* (pointer SDL_MessageBoxData))
(c-define-type* SDL_mutex (struct "SDL_mutex"))
(c-define-type SDL_PowerState int) ; enum
(c-define-type* SDL_Renderer (struct "SDL_Renderer"))
(c-define-type SDL_RendererFlip int) ; enum
(c-define-type* SDL_RWops (struct "SDL_RWops"))
(c-define-type SDL_Scancode int) ; enum
(c-define-type* SDL_sem (struct "SDL_sem"))
(c-define-type* SDL_SpinLock int)
(c-define-type SDL_SystemCursor int) ; enum
(c-define-type SDL_Texture (struct "SDL_Texture"))
(c-define-type SDL_Texture* (pointer SDL_Texture))
(c-define-type SDL_Thread (struct "SDL_Thread"))
(c-define-type SDL_Thread* (pointer SDL_Thread))
(c-define-type SDL_threadID unsigned-long)
(c-define-type SDL_ThreadPriority int) ; enum
(c-define-type SDL_TLSID unsigned-int)
(c-define-type SDL_TimerID int)
(c-define-type SDL_TouchID int64)
(c-define-type* SDL_Window (struct "SDL_Window"))

(c-define-type* SDL_AudioCVT (struct "SDL_AudioCVT"))
(c-define-type* SDL_AudioSpec (struct "SDL_AudioSpec"))
(c-define-type* SDL_Color (struct "SDL_Color"))
(c-define-type SDL_ControllerAxisEvent (struct "SDL_ControllerAxisEvent"))
(c-define-type SDL_ControllerButtonEvent (struct "SDL_ControllerButtonEvent"))
(c-define-type SDL_ControllerDeviceEvent (struct "SDL_ControllerDeviceEvent"))
(c-define-type* SDL_DisplayMode (struct "SDL_DisplayMode"))
(c-define-type SDL_DollarGestureEvent (struct "SDL_DollarGestureEvent"))
(c-define-type SDL_DropEvent (struct "SDL_DropEvent"))
(c-define-type* SDL_Event (union "SDL_Event"))
(c-define-type* SDL_Finger (struct "SDL_Finger"))
(c-define-type SDL_HapticCondition (struct "SDL_HapticCondition"))
(c-define-type SDL_HapticConstant (struct "SDL_HapticConstant"))
(c-define-type SDL_HapticCustom (struct "SDL_HapticCustom"))
(c-define-type SDL_HapticDirection (struct "SDL_HapticDirection"))
(c-define-type SDL_HapticEffect (union "SDL_HapticEffect"))
(c-define-type SDL_HapticLeftRight (struct "SDL_HapticLeftRight"))
(c-define-type SDL_HapticPeriodic (struct "SDL_HapticPeriodic"))
(c-define-type SDL_HapticRamp (struct "SDL_HapticRamp"))
(c-define-type SDL_JoyAxisEvent (struct "SDL_JoyAxisEvent"))
(c-define-type SDL_JoyBallEvent (struct "SDL_JoyBallEvent"))
(c-define-type SDL_JoyButtonEvent (struct "SDL_JoyButtonEvent"))
(c-define-type SDL_JoyDeviceEvent (struct "SDL_JoyDeviceEvent"))
(c-define-type SDL_JoyHatEvent (struct "SDL_JoyHatEvent"))
(c-define-type SDL_KeyboardEvent (struct "SDL_KeyboardEvent"))
(c-define-type SDL_KeyboardEvent* (pointer SDL_KeyboardEvent))
(c-define-type SDL_Keysym (struct "SDL_Keysym"))
(c-define-type SDL_Keysym* (pointer SDL_Keysym))
(c-define-type* SDL_MouseButtonEvent (struct "SDL_MouseButtonEvent"))
(c-define-type* SDL_MouseMotionEvent (struct "SDL_MouseMotionEvent"))
(c-define-type* SDL_MouseWheelEvent (struct "SDL_MouseWheelEvent"))
(c-define-type* SDL_MultiGestureEvent (struct "SDL_MultiGestureEvent"))
(c-define-type SDL_Palette (struct "SDL_Palette"))
(c-define-type SDL_Palette* (pointer SDL_Palette))
(c-define-type* SDL_PixelFormat (struct "SDL_PixelFormat"))
(c-define-type* SDL_Point (struct "SDL_Point"))
(c-define-type* SDL_QuitEvent (struct "SDL_QuitEvent"))
(c-define-type* SDL_Rect (struct "SDL_Rect"))
(c-define-type* SDL_RendererInfo (struct "SDL_RendererInfo"))
(c-define-type* SDL_Surface (struct "SDL_Surface"))
(c-define-type* SDL_SysWMEvent (struct "SDL_SysWMEvent"))
(c-define-type* SDL_SysWMinfo (struct "SDL_SysWMinfo"))
(c-define-type* SDL_SysWMmsg (struct "SDL_SysWMmsg"))
(c-define-type* SDL_TextEditingEvent (struct "SDL_TextEditingEvent"))
(c-define-type* SDL_TextInputEvent (struct "SDL_TextInputEvent"))
(c-define-type* SDL_TouchFingerEvent (struct "SDL_TouchFingerEvent"))
(c-define-type* SDL_UserEvent (struct "SDL_UserEvent"))
(c-define-type* SDL_WindowEvent (struct "SDL_WindowEvent"))
(c-define-type* SDL_assert_data (struct "SDL_assert_data"))
;;(c-define-type SDL_atomic_t (struct "SDL_atomic_t"))
(c-define-type* SDL_version (struct "SDL_version"))

(c-define-type* SDL_EventFilter (function (void* SDL_Event*) int))
(c-define-type SDL_HintCallback (function (void* nonnull-char-string nonnull-char-string nonnull-char-string) void))
(c-define-type* SDL_LogOutputFunction (function (void* int SDL_LogPriority nonnull-char-string) void))
(c-define-type SDL_TimerCallback (function (unsigned-int32 void*) unsigned-int32))
(c-define-type SDL_ThreadFunction (function (void*) int))
