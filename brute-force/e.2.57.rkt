#lang racket

(require (submod "e.2.56.rkt" export))

(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
      (cons '+ (cddr s))))

(define (multiplicand p)
  (if (null? (cdddr p))
      (caddr p)
      (cons '* (cddr p))))

(define (exponent p)
  (if (null? (cdddr p))
      (caddr p)
      (cons '** (cddr p))))

(define-unit contructors-general-alg-form@
  (import)
  (export constructors-algebraic-form^)
  (define (make-sum a1 a2 . rest)
    (append (list '+ a1 a2 ) rest))
  (define (make-product m1 m2 . rest)
    (append (list '* m1 m2) rest))
  (define (make-exponentiation m1 m2 . rest)
    (append (list '** m1 m2) rest)))

(module+ test
  ;; the predicates and ONLY some selectors remain the same
  (define-values/invoke-unit/infer selectors-alg-form@)
  (define-values/invoke-unit/infer predicates-alg-form@)
  ;; the constructors are different
  (define-values/invoke-unit/infer contructors-general-alg-form@)
  (define-values/invoke-unit/infer derivation@)

  (make-sum 1 2 'a 'b 'c)

  (make-sum 1 2 'a 'b (make-product 'x 2))

  (addend (make-sum 1 2 'a 'b 'c))

  (augend (make-sum 1 2 'a))

  (augend (make-sum 1 'a))

  (augend (make-sum 1 (make-product 'x 'y)))

  (make-product 'x 'y 'z)

  (multiplier (make-product 'x 'y 'z))

  (multiplicand (make-product 'x 'y))

  (multiplicand (make-product 'x 'y 3))

  (deriv '(+ x 3 x) 'x)

  (deriv '(* x 0 (+ x x x)) 'x)

  (deriv '(* 3 x y) 'x)

  (deriv '(+ (* x y) (+ x 3) (* 3 x y z)) 'x)

  (deriv '(* (* x y) (+ x 3) (* 3 x y z)) 'x))

