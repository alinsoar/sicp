#lang racket

;;; rational -> scheme-number -> complex

(require (prefix-in m: racket/main))
(require "sicp.rkt")
(require (submod "e.2.81.rkt" export))
(require (submod "e.2.80.rkt" export))
(require (submod "e.2.79.rkt" export))
(require (except-in (submod "e.2.77.rkt" export)
                    apply-generic@))
(require (submod "e.2.76.rkt" export))
(require (submod "e.2.83.rkt" export))
(require (only-in (submod "e.2.73.rkt" export)
                  table-operations^
                  table-operations@))
(require (submod "e.2.84.rkt" export))

(define-signature drop^
  (drop))
(define-signature GENERIC/project^
  (project))

(define-unit GENERIC/project@
  (import apply-generic^)
  (export GENERIC/project^)
  (define (project x) (apply-generic 'project x)))

;;; DROP is NOT GENERIC
(define-unit drop@
  (import apply-generic^ tags^ table-operations^ GENERIC/project^ GENERIC/equ?^)
  (export drop^)
  ;; We avoid using raise/project/apply-generic inside drop, because
  ;; our new apply-generic calls drop on the final result, and
  ;; sometimes we are not getting a final point in this recursion.
  (define (drop x)
    ;; project result as time as (equ? x (raise (project x))).
    (define (iter x co)
      (let ((p ((get 'project (list (type-tag x))) (contents x))))
        (cond ((false? p) (co x))
              ((equ? x ((get 'raise (list (type-tag p))) (contents p)))
               (co (iter p (lambda (y) y)))))))
    (iter x (lambda (x) x))))

(define-unit apply-generic@
  (import table-operations^ tags^ GENERIC/raise^ drop^)
  (export apply-generic^)
  (INVOKE (export (rename apply-generic/raise^
                          (--- apply-generic)))
          a/g/r:apply-generic)
  (define (apply-generic op . args)
    (let ((args (coerce-arguments-to-max-type args)))
      (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
          (if proc
              (let ((r (apply proc (map contents args))))
                (if (pair? r)
                    (drop r)            ; tagged data
                    r))                 ; scheme internals -- #f
              (error
               "No method for these types -- APPLY-GENERIC"
               (list op type-tags))))))))

(define-compound-unit/infer apply-generic/raise2@
  (import table-operations^ tags^)
  (export apply-generic^ drop^ GENERIC/project^ GENERIC/equ?^ GENERIC/raise^)
  (link   apply-generic@ drop@ GENERIC/project@ GENERIC/equ?@ GENERIC/raise@))

(define-unit extend-integer-package/project@
  (import table-operations^ tags^)
  (export)
  (put 'project '(integer) (lambda (x) false)))
(define-unit extend-scheme-number-package/project@
  (import table-operations^ tags^
          constructors/int/num/rat/complex-polar/complex-rect^)
  (export)
  (put 'project '(scheme-number) (lambda (x)
                                   (if (~0 (- x (round x)))
                                       (make-rational x 1)
                                       false))))
(define-unit extend-rational-package/project@
  (import table-operations^ tags^
          constructors/int/num/rat/complex-polar/complex-rect^)
  (export)
  (INVOKE install-rational-package)
  (put 'project '(rational)
       (lambda (x) (if (~= (denom x) 1)
                       (make-integer (numer x))
                       false))))
(define-unit extend-polar-package/project@
  (import table-operations^
          tags^
          constructors/int/num/rat/complex-polar/complex-rect^)
  (export)
  (INVOKE install-polar-package)
  (put 'project '(polar) (lambda (x)
                           (let* ((i (/ ((imag-part * m:sin) x) pi))
                                  (j (round i)))
                             (if (~0 (- i j))
                                 (make-scheme-number
                                  (* (if (odd? (inexact->exact j)) -1 1)
                                     ((real-part * m:cos) x)))
                                 false)))))
(define-unit extend-rectangular-package/project@
  (import table-operations^
          tags^
          constructors/int/num/rat/complex-polar/complex-rect^)
  (export)
  (INVOKE install-rectangular-package)
  (put 'project '(rectangular) (lambda (x)
                                 (if (~0 (imag-part x))
                                     (make-scheme-number (real-part x))
                                     false))))
(define-unit extend-complex-package/project@
  (import table-operations^
          tags^
          GENERIC/project^
          constructors/int/num/rat/complex-polar/complex-rect^)
  (export)
  (put 'project '(complex) (lambda (x) (project x))))

(module+ test

  (INVOKE install-tags-package)
  (INVOKE table-operations)
  
  (INVOKE apply-generic/raise2)

  (INVOKE install-basic-packages)

  (INVOKE extend-integer-package/raise)
  (INVOKE extend-rational-package/raise)
  (INVOKE extend-scheme-number-package/raise)
  (INVOKE extend-complex-package/raise)
  
  (INVOKE extend-integer-package/project)
  (INVOKE extend-rational-package/project)
  (INVOKE extend-scheme-number-package/project)
  (INVOKE extend-polar-package/project)
  (INVOKE extend-rectangular-package/project)
  (INVOKE extend-complex-package/project)
  
  (INVOKE extend-scheme-number-package/equ?)
  (INVOKE extend-rational-package/equ?)
  (INVOKE extend-complex-package/equ?)

  (get-tables table-show (lambda (-) nil))
  "---"
  (make-complex-from-real-imag 1 0)
  (make-rational 1 2)
  (raise (make-rational 1 2))
  (raise (raise (make-rational 1 2)))
  (make-complex-from-mag-ang 2 0)
  "---"
  (project (make-rational 1 2))
  (project (make-scheme-number 2))
  "---@-"
  (project (make-complex-from-mag-ang 2 0))
  (project (make-complex-from-mag-ang 2 pi))
  (project (make-complex-from-mag-ang 2 (* 3 pi)))
  (project (make-complex-from-mag-ang 2 (* 4 pi)))
  "---@-"
  (project (make-complex-from-real-imag 2 0))
  (project (make-complex-from-real-imag 2 1))
  (drop (make-complex-from-mag-ang 2 0))
  (drop (make-complex-from-mag-ang 2 pi))
  (drop (make-complex-from-mag-ang 2 (* 3 pi)))
  (drop (make-complex-from-mag-ang 2 (* 4 pi)))
  "---"
  (add (make-complex-from-real-imag 1 0)
       (make-rational 1 2))
  (add (make-complex-from-real-imag 1 0)
       (make-complex-from-mag-ang 1 0))
  (add (make-scheme-number 1)
       (make-scheme-number 1))
  "---"
  (drop (make-complex-from-real-imag 3 23))
  (drop (make-complex-from-real-imag 3 0))
  (drop (make-complex-from-real-imag 3.1 0))
  (drop (make-scheme-number 3.1))
  (drop (make-scheme-number 3))
  (drop (make-rational 3 2))
  "---"
  (project (make-complex-from-real-imag 3 23))
  (project (make-complex-from-real-imag 3 0))
  (project (make-complex-from-real-imag 3.1 0))
  (project (make-scheme-number 3.1))
  (project (make-scheme-number 3))
  (project (make-rational 3 2))
  "---"
  (mul (make-complex-from-real-imag 3 0)
       (add (make-complex-from-real-imag 1 0)
            (make-complex-from-real-imag 1 0)))
  (mul (make-complex-from-real-imag 1 0)
       (make-rational 2 1))
  'ok)

