#lang racket

(require "sicp.rkt")
(require (only-in (submod "e.2.73.rkt" export)
                  table-operations^
                  table-operations@))
(require (submod "e.2.76.rkt" export))
(require (submod "e.2.77.rkt" export))
(require (submod "e.2.84.rkt" export))
(require (submod "e.2.80.rkt" export))
(require (submod "e.2.81.rkt" export))
(require (submod "e.2.90.rkt" export))
(require (submod "e.2.89.rkt" export))
(require (submod "e.2.88.rkt" export))
(require (except-in (submod "e.2.87.rkt" export)
                    extend-complex-package/raise@))
(require (except-in (submod "e.2.83.rkt" export)
                    extend-complex-package/raise@))

(require racket/trace)

(define-signature arith-polynomial/div^
  (div-poly div-terms))
(define-signature div-cons^
  (div-quotient div-remainder))

;;; __IMPORTANT__: The function DIV works ONLY FOR UNIVARIATE
;;; POLYNOMIALS. It does not work for MULTIVARIATE polynomials,
;;; because it cannot deal with the rest of division of subterms in
;;; div-terms. The system should implement the type RATIONAL-FUNCTION
;;; in order to be able to eliminate the first term of the divisor of
;;; a multivariate polynomial at each iteration, for the recursive
;;; computation to reach the limit case of the zero divisor.

(define-unit arith-polynomial/div@
  (import apply-generic^ table-operations^ tags^
          polynomial-package-internals^
          GENERIC/add/sub/mul/div^
          GENERIC/neg^
          constructor/polynomial^
          constructor/sparse-polynomial^
          GENERIC/=zero?^)
  (export arith-polynomial/div^)
  (define div-terms
    (lambda (adjoin-term first-term clean-poly)
      (INVOKE (export (prefix a/m: arith-polynomial/add/mul^))
              arith-polynomial/add/mul)
      (INVOKE (export (prefix n/s: extend-polynomial-package-internals/neg/sub^))
              extend-polynomial-package/neg/sub)
      (lambda (L1 L2)
        (let ((mul-terms (a/m:mul-terms adjoin-term first-term))
              (sub-terms (n/s:sub-terms adjoin-term first-term)))
          (define (iter L1 L2)
            (if (empty-termlist? L1)
                (list (the-empty-termlist) (the-empty-termlist))
                (let ((t1 (first-term L1))
                      (t2 (first-term L2)))
                  (if (> (order t2) (order t1))
                      (list (the-empty-termlist) L1)
                      (let ((new-c (div (coeff t1) (coeff t2)))
                            (new-o (- (order t1) (order t2))))
                        (let ((new-term (make-term new-o new-c)))
                          (let ((rest-of-result
                                 (clean-poly
                                  (sub-terms L1
                                             (mul-terms L2
                                                        (adjoin-term new-term '()))))))
                            (let ((r (iter rest-of-result L2)))
                              (list (adjoin-term new-term (car r))
                                    (cadr r))))))))))
          (and #f
               (current-prefix-out "@**<=")
               (current-prefix-in "@++=>")
               (trace iter))
          (iter L1 L2)))))
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((r (div-terms (term-list p1)
                            (term-list p2))))
          (cons (make-polynomial (variable p1) (car r))
                (make-polynomial (variable p1) (cadr r))))
        (error "Polys not in same var -- DIV-POLY"
               (list p1 p2))))
  'done)
(define-unit extend-polynomial-package/sparse/dense/div@
  (import apply-generic^ table-operations^ tags^
          constructor/sparse-polynomial^
          constructors/int/num/rat/complex-polar/complex-rect^
          GENERIC/add/sub/mul/div^
          constructor/polynomial^
          polynomial-package-internals^
          arith-polynomial/div^
          GENERIC/=zero?^)
  (export div-cons^)
  (INVOKE (export (prefix combined: polynomial-package-internals/sparse/dense^))
          install-polynomial-package/sparse/dense)
  (INVOKE (export (prefix sparse: phyla-polynomial-package-internals^))
          install-polynomial-package/sparse)
  (INVOKE (export (prefix dense: phyla-polynomial-package-internals^))
          install-polynomial-package/dense)

  (define (clean-zeros-poly first-term adjoin-term)
    (lambda (term-list)
      (define (iter term-list co)
        (cond ((null? term-list)
               (co '()))
              ((=zero? (coeff (first-term term-list)))
               (iter (rest-terms term-list)
                     (lambda (x) (co x))))
              (else (co term-list))))
      '(trace iter)
      (iter term-list (lambda (x) x))))
  (define clean-sparse-poly (clean-zeros-poly sparse:first-term sparse:adjoin-term))
  (define clean-dense-poly (clean-zeros-poly dense:first-term dense:adjoin-term))

  (define (div-quotient r)
    (cons (car r)
          (cons (cadr r)
                (cons (caddr r)
                      (cadddr r)))))
  (define (div-remainder r)
    (cons (car r)
          (cons (cadr r)
                (cons (caddr r)
                      (cadr (cdddr r))))))

  (put 'div '(sparse sparse)
       (lambda (tl1 tl2)
         '(d "*** sparse sparse ***")
         ((div-terms sparse:adjoin-term sparse:first-term clean-sparse-poly)
          tl1 tl2)))
  (put 'div '(dense dense)
       (lambda (tl1 tl2)
         '(d "*** dense dense ***")
         ((div-terms dense:adjoin-term dense:first-term clean-dense-poly)
          tl1 tl2)))
  (put 'div '(sparse dense)
       (lambda (tl1 tl2)
         '(d "*** sparse dense ***")
         ((div-terms sparse:adjoin-term sparse:first-term clean-sparse-poly)
          tl1 (combined:cast-from-dense-to-sparse tl2))))
  (put 'div '(dense sparse)
       (lambda (tl1 tl2)
         '(d "*** dense sparse ***")
         ((div-terms dense:adjoin-term dense:first-term clean-dense-poly)
          tl1 (combined:cast-from-sparse-to-dense tl2))))

  (put 'div '(polynomial polynomial) (combined:apply-op-on-poly 'div))
  'done)
(define-compound-unit/infer extend-polynomial-package/sparse/dense@
  (import apply-generic^ table-operations^ tags^
          constructor/sparse-polynomial^
          constructors/int/num/rat/complex-polar/complex-rect^
          GENERIC/add/sub/mul/div^
          constructor/polynomial^
          GENERIC/neg^
          GENERIC/=zero?^)
  (export div-cons^)
  (link extend-polynomial-package/sparse/dense/div@
        arith-polynomial/div@
        install-polynomial-package@))

(module+ test
  (begin
    (INVOKE install-tags-package)
    (INVOKE table-operations)
    (INVOKE apply-generic/raise)

    (INVOKE install-basic-packages)

    (INVOKE GENERIC/=zero?)
    (INVOKE GENERIC/negation)
    
    (INVOKE constructor/polynomial)
    (INVOKE constructors/sparse-polynomial)
    (INVOKE constructors/dense-polynomial)
    
    ((lambda ()
       (INVOKE install-polynomial-package/sparse/dense)
       (populate-table)))
    (INVOKE extend-polynomial-package/sparse/dense)
    
    (INVOKE extend-integer-package/=zero?)
    (INVOKE extend-scheme-number-package/=zero?)
    (INVOKE extend-rational-package/=zero?)
    (INVOKE extend-complex-package/=zero?)
    (INVOKE extend-polar-package/=zero?)
    (INVOKE extend-rectangular-package/=zero?)
    (INVOKE extend-polynomial-package/=zero?)

    (INVOKE extend-integer-package/raise)
    (INVOKE extend-rational-package/raise)
    (INVOKE extend-scheme-number-package/raise)
    (INVOKE extend-complex-package/raise)
    (INVOKE extend-polynomial-package/raise)

    (INVOKE extend-integer-package/negation)
    (INVOKE extend-scheme-number-package/negation)
    (INVOKE extend-rational-package/negation)
    (INVOKE extend-complex-rectangular-package/negation)
    (INVOKE extend-complex-polar-package/negation)
    (INVOKE extend-complex-package/negation))

  (get-tables table-show (lambda (-) nil))

  (define P1s (make-sparse-polynomial 'x
                                      `((1 ,(make-scheme-number 8)) 
                                        (0 ,(make-integer 13)))))
  (define P2s (make-sparse-polynomial 'x
                                      `((4 ,(make-scheme-number 3))
                                        (3 ,(make-integer 211))
                                        (1 ,(make-rational 11 8)) 
                                        (0 ,(make-scheme-number 131)))))
  (define P3s (make-sparse-polynomial 'x
                                      `((4 ,(make-scheme-number 3))
                                        (3 ,(make-integer 211))
                                        (1 ,(make-rational 9 8)) 
                                        (0 ,(make-scheme-number 131)))))
  (define P4s (make-dense-polynomial 'x
                                     `(,(make-scheme-number 2)
                                       ,(make-rational 7 4)
                                       ,(make-scheme-number 5))))

  (define P1d (make-dense-polynomial 'x
                                     (list (make-scheme-number 2)
                                           (make-scheme-number 5))))
  (define P2d (make-dense-polynomial 'x
                                     (list (make-scheme-number 2)
                                           (make-scheme-number 5))))   
  (define P3d (make-dense-polynomial 'x
                                     (list
                                      (make-integer 3)
                                      (make-scheme-number 5))))
  (define P4d (make-dense-polynomial 'x
                                     (list
                                      (make-integer 3)
                                      (make-integer 211)
                                      (make-integer 0)
                                      (make-integer 8) 
                                      (make-integer 131))))
  (define P5d (make-dense-polynomial 'x
                                     `(,(make-scheme-number 2)
                                       ,(make-rational 7 4)
                                       ,(make-scheme-number 5))))
  (define P6d (make-dense-polynomial 'x
                                     `(,(make-scheme-number 2)
                                       ,(make-rational 7 4)
                                       ,(make-scheme-number 15))))
  (define P7d (make-dense-polynomial 'x
                                     `(,(make-complex-from-mag-ang 2 (/ pi 2))
                                       ,(make-complex-from-real-imag 7 4)
                                       ,(make-rational 5 3))))
  
  ;; KNOWN BUG: does not work all the time for division with complex
  ;; numbers, because this will generate polynomials having
  ;; 0-coefficients, and multiplication by 0 does not work for complex
  ;; numbers due to `angle` which tries to compute ATAN(0,0).
  
  (div (div-quotient (div (div-quotient (div (div-quotient (div (mul (mul P1d P2d)
                                                                     (mul P3d P2s))
                                                                P2d))
                                             P2s))
                          P1d))
       P3d)
  (div (div-quotient (div (div-quotient (div (mul (mul P5d P2s)
                                                  P5d)
                                             P2s))
                          P5d))
       P5d)
  ;; the 1st is sparse => sparse
  (div (div-quotient (div (div-quotient (div (mul (mul P3s P2d)
                                                  P4s)
                                             P2d))
                          P4s))
       P3s)
  (div (div-quotient (div (div-quotient (div (mul (mul P5d P2d)
                                                  P4s)
                                             P2d))
                          P4s))
       P5d)
  (div-remainder (div (mul (mul P1d P2d)
                      P3d)
                 P2d))
  (div (div-quotient (div (mul P7d P2d) P2d)) P7d)
  (div (mul P1d P2d) P2d)
  (div (mul P1d P3d) P3d)
  (div (mul P1d P4d) P4d)
  (div P5d P6d)
  (div-quotient (div P5d P6d))
  (div-remainder (div P5d P6d))
  'done)

