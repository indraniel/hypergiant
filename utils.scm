;;;; utils.scm

;;;; Small functions that don't have a home elsewhere
;;;; Imported by hypergiant.scm

(export update-string-mesh!
        make-string-mesh
        make-rgb-color make-rgba-color
        color-r color-g color-b color-a
        color-r-set! color-g-set! color-b-set! color-a-set!
        rgb-color-set! rgba-color-set!
        rgb-color-set-color! rgba-color-set-color!
        black white
        add-light)

(import srfi-4 miscmacros)

;;; Strings
(define (update-string-mesh! mesh node string face)
  (if (and node (zero? (string-length string)))
      (glls:set-renderable-n-elements! (scene:node-data node) 0)
      (begin
        (string-mesh string face mesh: mesh)
        (when node
          (glls:set-renderable-n-elements! (scene:node-data node)
                                           (mesh-n-indices mesh))))))

(define (make-string-mesh n-chars)
  (make-mesh vertices: `(attributes: ((position #:float 2)
                                      (tex-coord #:unsigned-short 2
                                                 normalized: #t))
                         n-vertices: ,(* n-chars 4))
             indices: `(type: #:ushort
                        n-indices: ,(* n-chars 6))))

;;; Colors
(define (make-rgb-color r g b #!optional non-gc?)
  (let ((v (make-f32vector 3 0 non-gc?)))
    (f32vector-set! v 0 (exact->inexact r))
    (f32vector-set! v 1 (exact->inexact g))
    (f32vector-set! v 2 (exact->inexact b))
    v))

(define (make-rgba-color r g b a #!optional non-gc?)
  (let ((v (make-f32vector 4 0 non-gc?)))
    (f32vector-set! v 0 (exact->inexact r))
    (f32vector-set! v 1 (exact->inexact g))
    (f32vector-set! v 2 (exact->inexact b))
    (f32vector-set! v 3 (exact->inexact a))
    v))

(define (color-r c)
  (f32vector-ref c 0))

(define (color-g c)
  (f32vector-ref c 1))

(define (color-b c)
  (f32vector-ref c 2))

(define (color-a c)
  (f32vector-ref c 3))

(define (color-r-set! c r)
  (f32vector-set! c 0 (exact->inexact r)))

(define (color-g-set! c g)
  (f32vector-set! c 1 (exact->inexact g)))

(define (color-b-set! c b)
  (f32vector-set! c 2 (exact->inexact b)))

(define (color-a-set! c a)
  (f32vector-set! c 3 (exact->inexact a)))

(define (rgb-color-set! c r g b)
  (color-r-set! c (exact->inexact r))
  (color-g-set! c (exact->inexact g))
  (color-b-set! c (exact->inexact b)))

(define (rgba-color-set! c r g b a)
  (rgb-color-set! c r g b)
  (color-a-set! c a))

(define (rgb-color-set-color! c d)
  (color-r-set! c (color-r d))
  (color-g-set! c (color-g d))
  (color-b-set! c (color-b d)))

(define (rgba-color-set-color! c d)
  (rgb-color-set-color! c d)
  (color-a-set! c (color-a d)))

(define black (make-rgb-color 0 0 0 #t))
(define white (make-rgb-color 1 1 1 #t))

;;; Hyperscene
(define (add-light node color intensity . args)
  (let ((light (apply scene:add-light node color intensity args)))
    (if* (get-keyword position: args)
         (scene:set-node-position! light it))
    (if* (get-keyword radius: args)
         (scene:set-node-bounding-sphere! light it))
    light))
