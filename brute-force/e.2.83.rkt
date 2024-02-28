#lang racket

(require (submod "e.2.77.rkt" export))
(require (submod "e.2.76.rkt" export))
(require (only-in (submod "e.2.73.rkt" export)
                  table-operations^
                  table-operations@))

;;; rational -> scheme-number -> complex

(define-signature GENERIC/raise^
  (raise))

(define-unit GENERIC/raise@
  (import apply-generic^)
  (export GENERIC/raise^)
  (define (raise x) (apply-generic 'raise x)))

(define-unit extend-integer-package/raise@
  (import tags^ table-operations^
          constructors/int/num/rat/complex-polar/complex-rect^ )
  (export)
  (put 'raise '(integer)
       (lambda (x) (make-rational x 1)))
  'done)
(define-unit extend-scheme-number-package/raise@
  (import tags^ table-operations^
          constructors/int/num/rat/complex-polar/complex-rect^ )
  (export)
  (put 'raise '(scheme-number)
       (lambda (x) (make-complex-from-real-imag x 0)))
  'done)
(define-unit extend-rational-package/raise@
  (import tags^ table-operations^
          constructors/int/num/rat/complex-polar/complex-rect^)
  (export)
  (INVOKE install-rational-package)
  (put 'raise '(rational)
       (lambda (x) (make-scheme-number (/ (numer x) (denom x)))))
  'done)
(define-unit extend-complex-package/raise@
  (import tags^ table-operations^)
  (export)
  (put 'raise '(complex) (lambda (x) false))
  'done)

(module+ test
  (require rackunit)
  (INVOKE install-tags-package)
  (INVOKE table-operations)
  (INVOKE apply-generic)
  (INVOKE install-basic-packages)
  (INVOKE-GENERIC raise)
  
  (INVOKE extend-integer-package/raise)
  (INVOKE extend-rational-package/raise)
  (INVOKE extend-scheme-number-package/raise)
  (INVOKE extend-complex-package/raise)
  
  (magnitude (make-complex-from-real-imag 3 4))
  (add (make-scheme-number 10)
       (make-scheme-number 10))
  
  ;; cannot raise the complex numbers
  (raise (make-complex-from-real-imag 3 4))

  (raise (make-scheme-number 10))
  (make-integer 99)
  (raise (make-integer 99))
  (raise (raise (make-integer 99)))
  (raise (raise (raise (make-integer 99))))
  (raise (raise (raise (raise (make-integer 99)))))
  (make-rational 10 2)
  (raise (make-rational 10 2))
  (raise (raise (make-rational 10 2)))
  (raise (raise (raise (make-rational 10 2))))
  (check-exn exn:fail?
             (lambda ()
               (raise
                (raise
                 (raise
                  (raise (make-rational 10 2))))))))

(module+ export
  (provide GENERIC/raise^
           GENERIC/raise@
           extend-integer-package/raise@
           extend-rational-package/raise@
           extend-scheme-number-package/raise@
           extend-complex-package/raise@))
