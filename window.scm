(export start
        stop
        get-window-size
        ui
        resize-hooks)

(define *last-render-time* 0)
(define ui #f)
(define *ui-camera* #f)

(define (render)
    (%swap-buffers (%window))
    (gl:clear (bitwise-ior gl:+color-buffer-bit+ gl:+depth-buffer-bit+))
    (scene:activate-camera *ui-camera*)
    (scene:render-cameras)
    (scene:deactivate-camera *ui-camera*))

(define (move-ui-camera w h)
  (scene:set-camera-position! *ui-camera*
                              (make-point (/ w 2) (/ h -2) 1)))

(define resize-hooks (make-parameter (list move-ui-camera)))

(define (resize _ w h)
  (gl:viewport 0 0 w h)
  (for-each (cut <> w h) (resize-hooks))
  (scene:resize-cameras))

(define (start* width height title . args)
  (%init)
  (scene:init (lambda () (%get-window-size (%window))))
  (%window-size-callback resize)
  (%mouse-button-callback mouse-click)
  (cond-expand
    (macosx (apply %make-window width height title
                   samples: 2
                   context-version-major: 3
                   context-version-minor: 2
                   opengl-forward-compat: #t
                   opengl-profile: %+opengl-core-profile+
                   args))
    (else (apply %make-window width height title samples: 2 args)))
  (%%set-cursor-pos-callback (%window) #$hpgCursorPositionCallback)
  (%%set-scroll-callback (%window) #$hpgScrollCallback)
  (%%set-key-callback (%window) #$hpgKeyCallback)
  (%%set-char-callback (%window) #$hpgCharCallback)
  (gl:init)
  (gl:enable gl:+depth-test+)
  (gl:enable gl:+blend+)
  (gl:blend-func gl:+src-alpha+ gl:+one-minus-src-alpha+)
  (gl:enable gl:+cull-face+)
  (cond-expand
    ((not gles) (gl:enable gl:+multisample+))
    (else))

  (set! ui (scene:make-scene))
  (set! *ui-camera* (scene:make-camera #:ortho #:position ui))
  (scene:deactivate-camera *ui-camera*)
  (resize #f width height)
  (glls:compile-pipelines)
  ((get-keyword init: args (lambda () (lambda () #f))))
  (gc)
  (set! *last-render-time* (%get-time))
  (let ((update (get-keyword update: args (lambda () (lambda (delta) #f)))))
    (let loop ()
      (let* ((time (%get-time))
             (delta (- time *last-render-time*)))
        (update delta)
        (scene:update-scenes)
        (scene:activate-camera *ui-camera*)
        (scene:update-cameras)
        (scene:deactivate-camera *ui-camera*)
        (render)
        (check-error)
        (set! *last-render-time* time)
        (when (feature? csi:)
          (thread-yield!))
        (%poll-events))
      (unless (%window-should-close? (%window))
        (loop))))
  ((get-keyword cleanup: args (lambda () (lambda () #f))))
  (gc)
  (%destroy-window (%window))
  (%terminate))

(define (start width height title . args)
  (define start** (lambda () (apply start* width height title args)))
  (if (feature? csi:)
      (thread-start! start**)
      (start**)))

(define (stop)
  (%set-window-should-close (%window) #t))

(define (get-window-size)
  (%get-window-size (%window)))
