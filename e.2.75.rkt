#lang racket

(require (submod "e.1.06.rkt" export))

;;; rectangular representation of complex numbers
(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude) (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define (apply-generic op arg) (arg op))

;;; polar representation of complex numbers
(define (make-from-mag-ang m a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* m (cos a)))
          ((eq? op 'imag-part) (* m (sin a)))
          ((eq? op 'magnitude) m)
          ((eq? op 'angle) a)
          (else (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(module+ test
  (cons ((make-from-mag-ang ((make-from-real-imag 10 10) 'magnitude)
                            ((make-from-real-imag 10 10) 'angle))
         'real-part)
        ((make-from-mag-ang ((make-from-real-imag 10 10) 'magnitude)
                            ((make-from-real-imag 10 10) 'angle))
         'imag-part)))



