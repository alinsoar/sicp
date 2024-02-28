#lang racket

(require racket/gui)
;; (require picturing-programs)

(require (submod "e.2.48.rkt" export))
(require (submod "e.2.47.rkt" export))
(require (submod "e.2.46.rkt" export))

(define (rogers) (make-object bitmap% "Rogers_William.jpg"))

(define (copy-painter painter)
    (let ((width (send painter get-width))
          (height (send painter get-height)))
      (let ((drawing-context (new bitmap-dc% [bitmap (make-bitmap width height)])))
        (send drawing-context draw-bitmap painter 0 0)
        (send drawing-context get-bitmap))))

(define (set-coordinates drawing-context frame scale-x scale-y)
  (send drawing-context set-transformation
            (vector
             ;; logical unit-vectors and logical origin
             (vector (xcor-vect (edge1-frame frame))  ; x-unit
                     (ycor-vect (edge1-frame frame))
                     (xcor-vect (edge2-frame frame))  ; y-unit
                     (ycor-vect (edge2-frame frame))
                     (xcor-vect (origin-frame frame)) ; origin
                     (ycor-vect (origin-frame frame)))
             0 0               ; physical device origin
             scale-x scale-y   ; scaling x and y axis on physical device
             0                 ; rotation on physical device.
             )))

(define (picture->painter painter)
  (lambda (frame)
    (lambda (picture)
      (let ((drawing-context (new bitmap-dc% [bitmap (copy-painter painter)])))
        (let ((width (send picture get-width))
              (height (send picture get-height)))
          (set-coordinates drawing-context frame (/ 1.0 width ) (/ 1.0 height))
          (send drawing-context draw-bitmap picture 0 0)
          '(send drawing-context save-file "pict.png" 'png)
          (send drawing-context get-bitmap))))))

;;; due to the graphical interface of racket in which this code was
;;; developed, we have to provide the `painter` and `color` to
;;; segments->painter. The code from the book was written using
;;; mit-scheme, where the low level language of the graphical
;;; interface is not the same as in racket.
(define (segments->painter painter color . segment-list)
  (lambda (frame)
    (let ((drawing-context (new bitmap-dc% [bitmap (copy-painter painter)])))
      (set-coordinates drawing-context frame 1 1)
      (send drawing-context set-pen color 1e-10 'solid)
      (for-each
       (lambda (segment)
         (send drawing-context draw-line
               (xcor-vect (start-segment segment))
               (ycor-vect (start-segment segment))
               (xcor-vect (end-segment segment))
               (ycor-vect (end-segment segment))))
       segment-list)
      '(send drawing-context save-file "pict.png" 'png)
      (send drawing-context get-bitmap))))

(define (frame-x painter)
  (lambda (frame)
    ((segments->painter painter
                        "red"
                        (make-segment (make-vect 0 1) (make-vect 1 0))
                        (make-segment (make-vect 0 0) (make-vect 1 1)))
     frame)))

(define (frame-outline painter)
  (lambda (frame)
    ((segments->painter painter
                        "cyan"
                        (make-segment (make-vect 0 0) (make-vect 1 0))
                        (make-segment (make-vect 0 0) (make-vect 0 1))
                        (make-segment (make-vect 0 1) (make-vect 1 1))
                        (make-segment (make-vect 1 0) (make-vect 1 1)))
     frame)))

(define (frame-diamond painter)
  (lambda (frame)
    ((segments->painter painter
                        "magenta"
                        (make-segment (make-vect 1/2 0) (make-vect 0 1/2))
                        (make-segment (make-vect 0 1/2) (make-vect 1/2 1))
                        (make-segment (make-vect 1/2 1) (make-vect 1 1/2))
                        (make-segment (make-vect 1 1/2) (make-vect 1/2 0)))
     frame)))

(define (4-images painter)
  (lambda (frame)
    (let ((of (origin-frame frame))
          (e1/2 (scale-vect 1/2 (edge1-frame frame)))
          (e2/2 (scale-vect 1/2 (edge2-frame frame))))
      (let ((f1 (make-frame of
                            e1/2
                            e2/2))
            (f2 (make-frame (add-vect of e1/2)
                            e1/2
                            e2/2))
            (f3 (make-frame (add-vect of e2/2)
                            e1/2
                            e2/2))
            (f4 (make-frame (add-vect of (add-vect (edge1-frame frame)
                                                   (edge2-frame frame)))
                            (scale-vect -1 e1/2)
                            (scale-vect -1 e2/2))))
        (lambda (picture)
          (let ((drawing-context (new bitmap-dc% [bitmap (copy-painter painter)])))
            (let ((width (send picture get-width))
                  (height (send picture get-height)))
              (set-coordinates drawing-context f1 (/ 1.0 width ) (/ 1.0 height))
              (send drawing-context draw-bitmap picture 0 0)
              (set-coordinates drawing-context f2 (/ 1.0 width ) (/ .5 height))
              (send drawing-context draw-bitmap picture 0 0)
              (set-coordinates drawing-context f3 (/ .5 width ) (/ 1.0 height))
              (send drawing-context draw-bitmap picture 0 0)
              (set-coordinates drawing-context f4 (/ 1.0 width ) (/ 1.0 height))
              (send drawing-context draw-bitmap picture 0 0)
              '(send drawing-context save-file "pict.png" 'png)
              (send drawing-context get-bitmap))))))))

(define f (make-frame (make-vect 50 750) ; frame origin
                      (make-vect 400 -200)   ; 0x-axis unit vector
                      (make-vect 200 -400)))  ; 0y-axis unit vector

(define (empty-picture) (make-bitmap 800 800))

(module+ test
  (define fo (frame-outline (empty-picture)))

  (define fx (frame-x (fo f)))

  (define fd (frame-diamond (fx f)))

  (fd f)

  (define wbr (picture->painter (fo f)))

  ((wbr f) (rogers))

  (define wbrw (4-images (fo f)))

  ((wbrw f) (rogers)) )


