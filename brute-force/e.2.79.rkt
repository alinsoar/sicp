#lang racket

(require (only-in (submod "e.2.73.rkt" export)
                  table-operations^
                  table-operations@))
(require (submod "e.2.77.rkt" export))
(require (submod "e.2.76.rkt" export))

(define-signature GENERIC/equ?^
  (equ?))

(define (~= x y) (< (abs (- x y)) 1e-10) )

(define-unit GENERIC/equ?@
  (import apply-generic^)
  (export GENERIC/equ?^)
  (define (equ? x y) (apply-generic 'equ? x y)))

(define-unit extend-scheme-number-package/equ?@
  (import tags^ apply-generic^ table-operations^)
  (export)
  (INVOKE install-rational-package)
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (~= x y)))
  (put 'equ? '(scheme-number rational)
       (lambda (x y) (and (~= x (/ (numer y) (denom y))))))
  (put 'equ? '(scheme-number complex)
       (lambda (x y) (and (~= 0 (apply-generic 'imag-part y))
                          (~= x (apply-generic 'real-part y)))))
  'done)
(define-unit extend-rational-package/equ?@
  (import tags^ apply-generic^ table-operations^)
  (export)
  (INVOKE install-rational-package)
  (put 'equ? '(rational rational)
       (lambda (x y)
         (and (~= (numer x) (numer y))
              (~= (denom x) (denom y)))))
  (put 'equ? '(rational scheme-number)
       (lambda (x y)
         (and (~= 1 (denom x))
              (~= y (numer x)))))
  (put 'equ? '(rational complex)
       (lambda (x y)
         ;; at this point y is either polar or rectangular, and we can
         ;; apply real-part and imag-part on y
         (and (~= 0 (apply-generic 'imag-part y))
              (~= (/ (numer x) (denom x))
                  (apply-generic 'real-part y)))))
  ;; no need to add rectangular and polar cases here, because they are
  ;; internal to complex, not visible from ouside
  'done)
(define-unit extend-complex-package/equ?@
  (import tags^ apply-generic^ table-operations^)
  (export)
  (INVOKE install-rational-package)
  (put 'equ? '(complex complex)
       (lambda (x y) (and (~= (apply-generic 'real-part x)
                              (apply-generic 'real-part y))
                          (~= (apply-generic 'imag-part x)
                              (apply-generic 'imag-part y)))))
  (put 'equ? '(complex scheme-number)
       (lambda (x y) (and (~= 0 (apply-generic 'imag-part x))
                          (~= y (apply-generic 'real-part x)))))
  (put 'equ? '(complex rational)
       (lambda (x y)
         (and (~= 0 (apply-generic 'imag-part x))
              (~= (/ (numer y) (denom y))
                  (apply-generic 'real-part x)))))
  'done)

(module+ test
  (INVOKE install-tags-package)
  (INVOKE table-operations)
  (INVOKE apply-generic)
  (INVOKE install-basic-packages)

  (INVOKE extend-scheme-number-package/equ?)
  (INVOKE extend-rational-package/equ?)
  (INVOKE extend-complex-package/equ?)
  (INVOKE-GENERIC equ?)

  (define (equ0? x y)
    (format "~a =? ~a => ~a" x y (equ? x y)))
  
  ;; rational -- rational
  (equ0? (make-rational 1 2)
        (make-rational 3 4))

  (equ0? (make-rational 1 2)
        (make-rational 1 2))

  ;; rational -- scheme-number
  (equ0? (make-rational 10 1)
        (make-scheme-number 10))

  (equ0? (make-rational 1 2)
        (make-scheme-number 10))

  ;; rational -- complex
  (equ0? (make-rational 1 2)
        (make-complex-from-real-imag 1 2))

  (equ0? (make-rational 1 2)
        (make-complex-from-real-imag 1/2 0))

  (equ0? (make-rational 1 2)
        (make-complex-from-real-imag .5 0))

  (equ0? (make-rational 1 3)
        (make-complex-from-real-imag .5 0))

  (equ0? (make-rational 20 2)
        (make-complex-from-mag-ang 10 0))

  (equ0? (make-rational 20 2)
        (make-complex-from-mag-ang 10 2))

  ;; should be true -- implementation detail
  (equ0? (make-rational 20 2)
        (make-complex-from-mag-ang 10 pi))

  ;; scheme-number -- rational
  (equ0? (make-scheme-number 10)
        (make-rational 10 1))

  (equ0? (make-scheme-number 10)
        (make-rational 10 2))

  (equ0? (make-scheme-number 5)
        (make-rational 10 2))

  ;; scheme-number -- scheme-number
  (equ0? (make-scheme-number 5)
        (make-scheme-number 5))

  (equ0? (make-scheme-number 5)
        (make-scheme-number 6))

  ;; scheme-number -- complex
  (equ0? (make-scheme-number 1)
        (make-complex-from-real-imag 1/2 0))

  (equ0? (make-scheme-number 1)
        (make-complex-from-real-imag 1 0))

  (equ0? (make-scheme-number 1)
        (make-complex-from-mag-ang 1 0))

  (equ0? (make-scheme-number 1)
        (make-complex-from-mag-ang 2 0))

  ;; complex -- scheme-number
  (equ0? (make-complex-from-mag-ang 2 0)
        (make-scheme-number 1))

  (equ0? (make-complex-from-mag-ang 2 0)
        (make-scheme-number 2))

  ;; complex -- rational
  (equ0? (make-complex-from-real-imag 1 0)
        (make-rational 1 2))

  (equ0? (make-complex-from-real-imag 2 0)
        (make-rational 4 2))

  (equ0? (make-complex-from-real-imag 2 1)
        (make-rational 4 2))

  ;; complex -- complex
  (equ0? (make-complex-from-real-imag 1 1)
        (make-complex-from-mag-ang (sqrt 2) (/ pi 4)))

  (equ0? (make-complex-from-real-imag 1 1)
        (make-complex-from-mag-ang 2 (/ pi 4)))

  (equ0? (make-complex-from-real-imag 1 1)
        (make-complex-from-real-imag 1 2))

  (equ0? (make-complex-from-real-imag 1 1)
        (make-complex-from-real-imag 1 1))
  'ok
  )

(module+ export
  (provide ~=
           extend-scheme-number-package/equ?@
           extend-rational-package/equ?@
           extend-complex-package/equ?@
           GENERIC/equ?@
           GENERIC/equ?^))
