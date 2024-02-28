#lang racket

;;; rational -> scheme-number -> complex

(require "sicp.rkt")
(require (only-in (submod "e.2.73.rkt" export)
                  table-operations^
                  table-operations@))
(require (submod "e.2.76.rkt" export))
(require (submod "e.2.77.rkt" export))
(require (submod "e.2.83.rkt" export))
(require (submod "e.2.84.rkt" export))
(require (rename-in (submod "e.1.06.rkt" export)
                    (square m:square)))
(require (submod "e.2.81.rkt" export))
(require (prefix-in m: racket/main))

(define-signature GENERIC/atan^
  (atan))
(define-signature GENERIC/sqrt^
  (sqrt))
(define-signature GENERIC/square^
  (square))
(define-signature GENERIC/sin^
  (sin))
(define-signature GENERIC/cos^
  (cos))

(define-unit GENERIC/atan@
  (import apply-generic^)
  (export GENERIC/atan^)
  (define atan
    (lambda (x y) (apply-generic 'atan x y))))
(define-unit GENERIC/sqrt@
  (import apply-generic^)
  (export GENERIC/sqrt^)
  (define sqrt
    (lambda (x) (apply-generic 'sqrt x))))
(define-unit GENERIC/square@
  (import apply-generic^)
  (export GENERIC/square^)
  (define square
    (lambda (x) (apply-generic 'square x))))
(define-unit GENERIC/sin@
  (import apply-generic^)
  (export GENERIC/sin^)
  (define sin
    (lambda (x) (apply-generic 'sin x))))
(define-unit GENERIC/cos@
  (import apply-generic^)
  (export GENERIC/cos^)
  (define cos
    (lambda (x) (apply-generic 'cos x))))

'(
;;; in this exercise we modify the packages: RATIONAL, SCHEME-NUMBER,
;;; RECTANGULAR, POLAR and COMPLEX.
;;; in RATIONAL and SCHEME-NUMBER PACKAGES we only insert the function
;;; REAL-NUMBER-VALUE
;;; we need to re-define some internal definitions of each package so:
;;; in RECTANGULAR package: REAL-PART and IMAG-PART are redefined such
;;; that to return SCHEME-NUMBER instead of internal real number. The
;;; other functions are unchanged, but kept there only for their
;;; environment to link to the new definitions of REAL-PART and
;;; IMAG-PART.
;;; in POLAR package: redefined MAGNITUDE and ANGLE. The others kept
;;; there for the same reason as in the case of RECTANGULAR.
;;; in COMPLEX package: COMPLEX is the package built over POLAR and
;;; RECTANGULAR, so it uses directly the representations exported by
;;; these packages. Before, these packages exported over their
;;; abstraction barrier internal real numbers, now it exports
;;; SCHEME-NUMBER objects. Consequently, we replace * + - and / with
;;; MUL DIV ADD SUB, whose calls pass through the APPLY-GENERIC.
)

(define-unit extend-scheme-number-package/sin/cos@
  (import table-operations^ tags^)
  (export)
  (INVOKE install-scheme-number-package)
  (put 'sin '(scheme-number) (lambda (x) (tag (m:sin x))))
  (put 'cos '(scheme-number) (lambda (x) (tag (m:cos x))))
  'done)
(define-unit extend-rational-package/sin/cos@
  (import table-operations^ tags^
          constructors/int/num/rat/complex-polar/complex-rect^)
  (export)
  (INVOKE install-rational-package)
  (define real-number-value (lambda (x) (/ (numer x) (denom x))))
  (put 'cos '(rational)
       (lambda (x) (make-scheme-number (m:cos (real-number-value x)))))
  (put 'sin '(rational)
       (lambda (x) (make-scheme-number (m:sin (real-number-value x)))))
  'done)

(define-unit extend-scheme-number-package/sqrt@
  (import table-operations^ tags^)
  (export)
  (INVOKE install-scheme-number-package)
  (put 'sqrt '(scheme-number) (lambda (x) (tag (m:sqrt x))))
  'done)
(define-unit extend-rational-package/sqrt@
  (import table-operations^ tags^
          constructors/int/num/rat/complex-polar/complex-rect^)
  (export)
  (INVOKE install-rational-package)
  (put 'sqrt '(rational)
       (lambda (x)
         (make-scheme-number (m:sqrt (/ (numer x)
                                        (denom x))))))
  'done)

(define-unit extend-scheme-number-package/atan@
  (import table-operations^ tags^)
  (export)
  (INVOKE install-scheme-number-package)
  (put 'atan '(scheme-number scheme-number)
       (lambda (y x) (tag (m:atan y x))))
  'done)
(define-unit extend-rational-package/atan@
  (import table-operations^ tags^
          constructors/int/num/rat/complex-polar/complex-rect^)
  (export)
  (INVOKE install-rational-package)
  (put 'atan '(rational rational)
       (lambda (y x) (make-scheme-number (m:atan (/ (numer y)
                                                    (denom y))
                                                 (/ (numer x)
                                                    (denom x))))))
  'done)

(define-unit extend-scheme-number-package/square@
  (import table-operations^ tags^)
  (export)
  (INVOKE install-scheme-number-package)
  (put 'square '(scheme-number) (lambda (x) (tag (m:square x))))
  'done)
(define-unit extend-rational-package/square@
  (import table-operations^ tags^
          constructors/int/num/rat/complex-polar/complex-rect^)
  (export)
  (INVOKE install-rational-package)
  (put 'square '(rational)
       (lambda (x) (tag (cons (m:square (numer x))
                              (m:square (denom x))))))
  'done)

(define-unit install-rectangular-package/extended-complex@
  (import table-operations^
          tags^
          GENERIC/sqrt^ GENERIC/square^ GENERIC/sin^ GENERIC/cos^ GENERIC/atan^
          GENERIC/add/sub/mul/div^)
  (export populate-table^)
  (INVOKE install-rectangular-package
          (import table-operations^ tags^)
          (export (rename rectangular-package-internals^
                          (--- populate-table))))
  (define (populate-table)
    (install-with-ops mul add atan sin cos square sqrt))
  'done)
(define-unit install-polar-package/extended-complex@
  (import table-operations^
          tags^
          GENERIC/sqrt^ GENERIC/square^ GENERIC/sin^ GENERIC/cos^ GENERIC/atan^
          GENERIC/add/sub/mul/div^
          constructors/int/num/rat/complex-polar/complex-rect^)
  (export populate-table^)
  ;; interface to the rest of the system
  (INVOKE install-polar-package
          (import table-operations^ tags^)
          (export (rename polar-package-internals^
                          (--- populate-table))))
  (define (populate-table)
    (install-with-ops add sub mul sqrt square atan sin cos))
  'done)
(define-unit install-complex-package/extended-complex@
  (import table-operations^
          tags^
          apply-generic^
          GENERIC/add/sub/mul/div^
          GENERIC/mag/ang/real-part/imag-part/conj^)
  (export populate-table^)
  (INVOKE install-complex-package
          (import apply-generic^
                  table-operations^
                  tags^
                  GENERIC/mag/ang/real-part/imag-part/conj^)
          (export (rename complex-package-internals^
                          (--- populate-table))))
  (define (populate-table)
    (install-with-ops add sub mul div))
  'done)

(module+ test
  (begin
    (INVOKE install-tags-package)
    (INVOKE table-operations)
    (INVOKE apply-generic/raise)
    
    (INVOKE-GENERIC mag/ang/real-part/imag-part/conj)
    (INVOKE-GENERIC add/sub/mul/div)
    (INVOKE-GENERIC sin)
    (INVOKE-GENERIC cos)
    (INVOKE-GENERIC sqrt)
    (INVOKE-GENERIC square)
    (INVOKE-GENERIC atan)
    
    (INVOKE constructors/int/num/rat/complex-polar/complex-rect)

    ((lambda () (INVOKE install-scheme-number-package)
       (populate-table)))
    ((lambda () (INVOKE install-rational-package)
       (populate-table)))
    
    (INVOKE extend-scheme-number-package/sin/cos)
    (INVOKE extend-rational-package/sin/cos)
    
    (INVOKE extend-scheme-number-package/square)
    (INVOKE extend-rational-package/square)
    
    (INVOKE extend-scheme-number-package/sqrt)
    (INVOKE extend-rational-package/sqrt)
    
    (INVOKE extend-scheme-number-package/atan)
    (INVOKE extend-rational-package/atan)
    
    ((lambda () (INVOKE install-rectangular-package/extended-complex)
             (populate-table)))
    ((lambda () (INVOKE install-polar-package/extended-complex)
             (populate-table)))
    ((lambda () (INVOKE install-complex-package/extended-complex)
             (populate-table)))
    
    (INVOKE extend-rational-package/raise)
    (INVOKE extend-scheme-number-package/raise)
    (INVOKE extend-complex-package/raise))

  (get-tables table-show (lambda (-) nil))
  "---"
  (sub
   (atan (make-scheme-number 2)
         (make-scheme-number 2))
   (make-rational pi 4))
  ((lambda (k)
     (list
      k
      (sin (make-rational pi k))
      (cos (make-rational pi k))
      (atan (sin (make-rational pi k))
             (cos (make-rational pi k)))
      (sub
       (make-rational pi k)
       (atan (sin (make-rational pi k))
             (cos (make-rational pi k))))))
   3)
  "---"
  (list (list
         (cos (make-rational 4 2))
         (cos (make-rational pi 2))
         (cos (make-rational pi 3))
         (cos (make-rational pi 1))
         
         (sin (make-rational 4 2))
         (sin (make-rational pi 2))
         (sin (make-rational pi 6))
         (sin (make-rational pi 1)))
        (list
         (cos (make-scheme-number (/ 4 2)))
         (cos (make-scheme-number (/ pi 2)))
         (cos (make-scheme-number (/ pi 3)))
         (cos (make-scheme-number (/ pi 1)))

         (sin (make-scheme-number (/ 4 2)))
         (sin (make-scheme-number (/ pi 2)))
         (sin (make-scheme-number (/ pi 6)))
         (sin (make-scheme-number (/ pi 1)))))
  "---"
  (square (make-rational 3 4))
  (square (make-scheme-number 15))

  (sqrt (make-rational 32 2))
  (sqrt (make-scheme-number 256))
  
  (make-complex-from-real-imag
   (make-scheme-number 4.2)
   (make-rational 4 2))

  (add (make-scheme-number 4.2)
       (make-rational 4 2))
  
  (magnitude (make-complex-from-real-imag (make-scheme-number 4.2)
                                          (make-rational 3 2)))
  
  (angle (make-complex-from-real-imag (make-scheme-number 4.2)
                                      (make-rational 3 2)))

  (real-part (make-complex-from-real-imag (make-scheme-number 4.2)
                                          (make-rational 3 2)))

  (imag-part (make-complex-from-real-imag (make-scheme-number 4.2)
                                          (make-rational 3 2)))

  (magnitude (make-complex-from-mag-ang (make-scheme-number 4.2)
                                        (make-rational 3 2)))

  (angle (make-complex-from-mag-ang (make-scheme-number 4.2)
                                    (make-rational 3 2)))

  (real-part (make-complex-from-mag-ang (make-scheme-number 4.2)
                                        (make-rational 3 2)))

  (imag-part (make-complex-from-mag-ang (make-scheme-number 4.2)
                                        (make-rational 3 2)))

  (angle
   (add
    (mul (make-complex-from-mag-ang (make-scheme-number 3)
                                    (make-rational pi 2))
         (make-complex-from-mag-ang (make-scheme-number 4)
                                    (make-rational pi 2)))
    (add
     (make-complex-from-real-imag (make-scheme-number 4.2)
                                  (make-rational 3 2))
     (make-complex-from-real-imag (make-scheme-number 4.2)
                                  (make-rational 3 2)))))

  'done)


