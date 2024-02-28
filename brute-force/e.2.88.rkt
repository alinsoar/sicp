#lang racket

(require "sicp.rkt")
(require (only-in (submod "e.2.73.rkt" export)
                  table-operations^ table-operations@))
(require (submod "e.2.84.rkt" export))
(require (submod "e.2.76.rkt" export))
(require (submod "e.2.77.rkt" export))
(require (submod "e.2.80.rkt" export))
(require (submod "e.2.81.rkt" export))
(require (submod "e.2.87.rkt" export))
(require (except-in (submod "e.2.83.rkt" export)
                    extend-complex-package/raise@))

(define-signature GENERIC/neg^
  (neg))
(define-signature GENERIC/sub^
  (sub))
(define-signature extend-polynomial-package-internals/neg/sub^
  (sub-terms))

(define-unit GENERIC/negation@
  (import apply-generic^)
  (export GENERIC/neg^)
  (define (neg x) (apply-generic 'neg x)))
(define-unit GENERIC/sub@
  (import apply-generic^)
  (export GENERIC/sub^)
  (define (sub x) (apply-generic 'sub x)))

(define-unit extend-scheme-number-package/negation@
  (import tags^ table-operations^ apply-generic^)
  (export)
  (INVOKE install-scheme-number-package)
  (put 'neg '(scheme-number)
       (lambda (x) (tag (- x))))
  'done)
(define-unit extend-integer-package/negation@
  (import tags^ table-operations^ apply-generic^)
  (export)
  (INVOKE install-integer-package)
  (put 'neg '(integer) (lambda (x) (tag (- x))))
  'done)
(define-unit extend-rational-package/negation@
  (import tags^ table-operations^ apply-generic^
          constructors/int/num/rat/complex-polar/complex-rect^)
  (export)
  (INVOKE install-rational-package)
  (put 'neg '(rational)
       (lambda (x) (make-rational (- (numer x)) (denom x))))
  'done)
(define-unit extend-complex-rectangular-package/negation@
  (import tags^ table-operations^ apply-generic^
          GENERIC/neg^
          constructors/int/num/rat/complex-polar/complex-rect^)
  (export)
  (INVOKE install-rectangular-package)
  (put 'neg '(rectangular)
       (lambda (x) (make-complex-from-real-imag (- (real-part x))
                                                (- (imag-part x)))))
  'done)
(define-unit extend-complex-polar-package/negation@
  (import tags^ table-operations^ apply-generic^
          GENERIC/neg^
          constructors/int/num/rat/complex-polar/complex-rect^)
  (export)
  (INVOKE install-polar-package)
  (put 'neg '(polar)
       (lambda (x) (make-complex-from-mag-ang (magnitude x)
                                              (+ pi (angle x)))))
  'done)
(define-unit extend-complex-package/negation@
  (import tags^ table-operations^ apply-generic^ GENERIC/neg^ )
  (export)
  (put 'neg '(complex)
       (lambda (x) (neg x)))
  'done)

(define-unit extend-polynomial-package/neg/sub@
  (import tags^ table-operations^ apply-generic^
          constructor/polynomial^
          GENERIC/add/sub/mul/div^
          GENERIC/neg^
          GENERIC/=zero?^)
  (export populate-table^
          extend-polynomial-package-internals/neg/sub^)
  (INVOKE (export (prefix p: polynomial-package-internals^)
                  (prefix p: arith-polynomial/add/mul^))
          install-polynomial-package)
  (define (negate-term t)
    (p:make-term (p:order t) (neg (p:coeff t))))
  (define (negate-terms first-term adjoin-term)
    (lambda (term-list)
      (letrec ((iter
                (lambda (l co)
                  (if (null? l)
                      (co '())
                      (iter (cdr l)
                            (lambda (x)
                              (co (cons (first-term l) x)))))))
               (terms (iter term-list (lambda (x) x))))
        (map (lambda (x)
               (car (adjoin-term (negate-term x) '())))
             terms))))
  (define (negate-poly first-term adjoin-term term-list)
    (lambda (p)
      (make-polynomial (p:variable p)
                       ((negate-terms first-term adjoin-term)
                        (term-list p)))))
  (define (sub-terms adjoin-term first-term)
    (lambda (L1 L2)
      ((p:add-terms adjoin-term first-term)
       ((negate-terms first-term adjoin-term) L2)
       L1)))
  (define (sub-poly first-term adjoin-term term-list)
    (lambda (p1 p2)
      (if (p:same-variable? (p:variable p1) (p:variable p2))
          (make-polynomial (p:variable p1)
                             ((sub-terms adjoin-term first-term)
                              (term-list p1)
                              (term-list p2)))
          (error "Polys not in same var -- ADD-POLY"
                 (list p1 p2)))))
  (define (populate-table first-term adjoin-term term-list)
    (put 'neg '(polynomial) (negate-poly first-term adjoin-term term-list))
    (put 'sub '(polynomial polynomial) (sub-poly first-term adjoin-term term-list)))
  'done)

(module+ test
  (INVOKE install-tags-package)
  (INVOKE table-operations)
  (INVOKE apply-generic/raise)

  (INVOKE install-basic-packages)
  
  (INVOKE GENERIC/negation)
  (INVOKE GENERIC/=zero?)
  
  (INVOKE constructor/polynomial)
  
  ((lambda ()
     (INVOKE (export (prefix p: polynomial-package-internals^))
             install-polynomial-package)
     (INVOKE (export (prefix s: phyla-polynomial-package-internals^)
                     (prefix s: populate-table^))
             install-polynomial-package/sparse)
     (INVOKE (export (prefix z: populate-table^))
             extend-polynomial-package/=zero?)
     (INVOKE (export (prefix neg/sub: populate-table^))
             extend-polynomial-package/neg/sub)
     (neg/sub:populate-table s:first-term s:adjoin-term p:term-list)
     (s:populate-table)
     
     (p:populate-table/constructor-poly)
     (z:populate-table s:first-term p:term-list)))
  
  (INVOKE extend-integer-package/=zero?)
  (INVOKE extend-scheme-number-package/=zero?)
  (INVOKE extend-rational-package/=zero?)
  (INVOKE extend-complex-package/=zero?)
  (INVOKE extend-polar-package/=zero?)
  (INVOKE extend-rectangular-package/=zero?)

  (INVOKE extend-integer-package/negation)
  (INVOKE extend-scheme-number-package/negation)
  (INVOKE extend-rational-package/negation)
  (INVOKE extend-complex-rectangular-package/negation)
  (INVOKE extend-complex-polar-package/negation)
  (INVOKE extend-complex-package/negation)

  (INVOKE extend-integer-package/raise)
  (INVOKE extend-rational-package/raise)
  (INVOKE extend-scheme-number-package/raise)
  (INVOKE extend-complex-package/raise)
  (INVOKE extend-polynomial-package/raise)

  (get-tables table-show (lambda (-) nil))

  (=zero? (mul (make-polynomial 'y `((2 ,(make-scheme-number 0))))
               (sub (sub
                     (make-polynomial
                      'y `((2 ,(make-rational 1 2))))
                     (make-polynomial
                      'y `((2 ,(make-rational 1 2)))))
                    (make-polynomial 'y '()))))

  (=zero? (sub (neg
                (make-polynomial
                 'y `((2 ,(make-polynomial
                           'x `((2 ,(make-rational 1 2))
                                (0 ,(make-scheme-number 3)))))
                      (1 ,(make-polynomial
                           'x `((2 ,(make-rational 1 2))
                                (0 ,(make-polynomial
                                     'z `((2 ,(make-integer 7)))))))))))
               (neg
                (make-polynomial
                 'y `((2 ,(make-polynomial
                           'x `((2 ,(make-rational 1 2))
                                (0 ,(make-scheme-number 3)))))
                      (1 ,(make-polynomial
                           'x `((2 ,(make-rational 1 2))
                                (0 ,(make-polynomial
                                     'z `((2 ,(make-integer 7)))))))))))))
  
  (=zero? (add
           (neg
            (make-polynomial 'x (list (list 2 (make-rational 1 2)))))
           (neg
            (neg
             (make-polynomial 'x (list (list 2 (make-rational 1 2))))))))
  
  (=zero? (sub (add
                (make-polynomial
                 'y `((2 ,(make-rational 1 2))))
                (make-polynomial
                 'y `((3 ,(neg (make-polynomial
                                'x `((2 ,(make-integer 2)))))))))
               (add
                (make-polynomial
                 'y `((2 ,(make-rational 1 2))))
                (make-polynomial
                 'y `((3 ,(neg (make-polynomial
                                'x `((2 ,(make-integer 2)))))))))))

  (=zero? (sub
           (make-polynomial
            'y `((2 ,(make-rational 1 2))))
           (make-polynomial
            'y `((2 ,(make-rational 1 2))))))
  'done)

(module+ export
  (provide extend-polynomial-package/neg/sub@
           extend-polynomial-package-internals/neg/sub^

           extend-scheme-number-package/negation@
           extend-integer-package/negation@
           extend-rational-package/negation@
           extend-complex-rectangular-package/negation@
           extend-complex-polar-package/negation@
           extend-complex-package/negation@
           
           GENERIC/neg^
           GENERIC/sub^
           GENERIC/negation@
           GENERIC/sub@))

