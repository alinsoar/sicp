#lang racket

(require (submod "e.2.46.rkt" export))

(define-signature frame-encoding^
  (make-frame origin-frame edge1-frame edge2-frame))

(define-signature frame-tool^
  (str-frame frame-coord-map))

(define-unit first-frame-encoding@
  (import)
  (export frame-encoding^)
  (define (make-frame origin edge1 edge2)
    (list origin edge1 edge2))
  (define origin-frame car)
  (define edge1-frame cadr)
  (define edge2-frame caddr))

(define-unit second-frame-encoding@
  (import)
  (export frame-encoding^)
  (define (make-frame origin edge1 edge2)
    (cons origin (cons edge1 edge2)))
  (define origin-frame car)
  (define edge1-frame cadr)
  (define edge2-frame cddr))

(define-unit frame-tool@
  (import frame-encoding^)
  (export frame-tool^)
  (define (str-frame f)
    (format "origin=[~a;~a] unitx=[~a;~a] unity=[~a;~a]"
            (xcor-vect (origin-frame f))
            (ycor-vect (origin-frame f))
            (xcor-vect (edge1-frame f))
            (ycor-vect (edge1-frame f))
            (xcor-vect (edge2-frame f))
            (ycor-vect (edge2-frame f))))

  (define (frame-coord-map frame)
    (lambda (v)
      (add-vect
       (origin-frame frame)
       (add-vect (scale-vect (xcor-vect v)
                             (edge1-frame frame))
                 (scale-vect (ycor-vect v)
                             (edge2-frame frame)))))))

(module+ test
  (define (test1)
    (define-values/invoke-unit/infer first-frame-encoding@)
    (define-values/invoke-unit/infer frame-tool@)
    (define v0 (make-vect 10 2))
    (define vx (make-vect 4 0))
    (define vy (make-vect 0 4))
    (define f1 (make-frame v0 vx vy))
    (str-frame f1)
    (xcor-vect ((frame-coord-map f1) (make-vect 1 1)))
    (ycor-vect ((frame-coord-map f1) (make-vect 1 1)))
    (define f2 (make-frame v0 vx vy))
    (str-frame f2)
    (xcor-vect ((frame-coord-map f2) (make-vect 1 1)))
    (ycor-vect ((frame-coord-map f2) (make-vect 1 1))))
  (test1))

(module+ test
 (define (test2)
   (define-values/invoke-unit/infer second-frame-encoding@)
   (define-values/invoke-unit/infer frame-tool@)
   (define v0 (make-vect 10 2))
   (define vx (make-vect 4 0))
   (define vy (make-vect 0 4))
   (define f1 (make-frame v0 vx vy))
   (str-frame f1)
   (xcor-vect ((frame-coord-map f1) (make-vect 1 1)))
   (ycor-vect ((frame-coord-map f1) (make-vect 1 1)))
   (define f2 (make-frame v0 vx vy))
   (str-frame f2)
   (xcor-vect ((frame-coord-map f2) (make-vect 1 1)))
   (ycor-vect ((frame-coord-map f2) (make-vect 1 1))))
 (test2))

(module+ export
  (define-values/invoke-unit/infer second-frame-encoding@)
  (define-values/invoke-unit/infer frame-tool@)
  (provide frame-coord-map
           make-frame
           origin-frame
           edge1-frame
           edge2-frame))
