#lang racket

(require "sicp.rkt")
(require (only-in (submod "e.2.73.rkt" export)
                  table-operations^
                  table-operations@))
(require (submod "e.2.76.rkt" export))
(require (submod "e.2.77.rkt" export))
(require (submod "e.2.79.rkt" export))
(require (only-in (submod "e.2.81.rkt" export)
                  get-tables))

(define (~0 x) (~= x 0))

(define-signature GENERIC/=zero?^
  (=zero?))

(define-unit GENERIC/=zero?@
  (import apply-generic^)
  (export GENERIC/=zero?^)
  (define (=zero? x) (apply-generic '=zero? x)))

(define-unit extend-integer-package/=zero?@
  (import table-operations^)
  (export)
  (put '=zero? '(integer)
       (lambda (x) (~0 x)))
  'done)
(define-unit extend-scheme-number-package/=zero?@
  (import table-operations^)
  (export)
  (put '=zero? '(scheme-number)
       (lambda (x) (~0 x)))
  'done)
(define-unit extend-rational-package/=zero?@
  (import tags^ table-operations^)
  (export)
  (INVOKE install-rational-package)
  (put '=zero? '(rational)
       (lambda (x) (~0 (numer x))))
  'done)
(define-unit extend-rectangular-package/=zero?@
  (import apply-generic^ tags^ table-operations^ GENERIC/=zero?^)
  (export)
  (INVOKE install-rectangular-package)
  (put '=zero? '(rectangular)
       (lambda (x)
         (and (~0 (real-part x))
              (~0 (imag-part x)))))
  'done)
(define-unit extend-polar-package/=zero?@
  (import apply-generic^ tags^ table-operations^ GENERIC/=zero?^)
  (export)
  (INVOKE install-polar-package)
  (put '=zero? '(polar)
       (lambda (x)
         (and (~0 (magnitude x))
              (~0 (angle x)))))
  'done)
(define-unit extend-complex-package/=zero?@
  (import apply-generic^ table-operations^ GENERIC/=zero?^)
  (export)
  (put '=zero? '(complex) =zero?)
  'done)

(module+ test
  (INVOKE install-tags-package)
  (INVOKE table-operations)
  (INVOKE apply-generic)
  (INVOKE install-basic-packages)
  
  (INVOKE GENERIC/=zero?)
  
  (INVOKE extend-integer-package/=zero?)
  (INVOKE extend-scheme-number-package/=zero?)
  (INVOKE extend-rational-package/=zero?)
  (INVOKE extend-complex-package/=zero?)
  (INVOKE extend-polar-package/=zero?)
  (INVOKE extend-rectangular-package/=zero?)
  
  (get-tables table-show (lambda (-) nil))
  
  (map (lambda (x) (format "~a => ~a" x (=zero? x)))
       (list
        (make-scheme-number 10)
        (make-scheme-number 0)
        (make-rational 0 2)
        (make-rational 1 2)
        (make-integer 2)
        (make-integer 0)
        (make-complex-from-real-imag 0 0)
        (make-complex-from-real-imag 0 1)
        (make-complex-from-real-imag 1 0)
        (make-complex-from-mag-ang 0 0)
        (make-complex-from-mag-ang 0 100)
        (make-complex-from-mag-ang 1 1))))

(module+ export
  (provide ~0
           GENERIC/=zero?@
           GENERIC/=zero?^
           extend-complex-package/=zero?@
           extend-integer-package/=zero?@
           extend-rational-package/=zero?@
           extend-polar-package/=zero?@
           extend-rectangular-package/=zero?@
           extend-scheme-number-package/=zero?@))

