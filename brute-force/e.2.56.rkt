#lang racket

(define-signature constructors-algebraic-form^
  (make-sum make-product make-exponentiation))
(define-signature selectors-algebraic-form^
  (addend augend multiplier multiplicand exponent base))
(define-signature predicates-algebraic-form^
  (sum? product? exponentiation?))
(define-signature derivation^
  (deriv))

;;; general predicates that make use of internals of scheme
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define-unit selectors-alg-form@
  (import)
  (export selectors-algebraic-form^)
  (define (addend s) (cadr s))
  (define (augend s) (caddr s))
  (define (multiplier p) (cadr p))
  (define (multiplicand p) (caddr p))
  (define exponent caddr)
  (define base cadr))
(define-unit predicates-alg-form@
  (import)
  (export predicates-algebraic-form^)
  (define (sum? x)
    (and (pair? x) (eq? (car x) '+)))
  (define (product? x)
    (and (pair? x) (eq? (car x) '*)))
  (define (exponentiation? exp)
    (and (pair? exp) (eq? (car exp) '**))))
(define-unit constructor-alg-form-without-simplification@
  (import)
  (export constructors-algebraic-form^)
  (define (make-sum a1 a2) (list '+ a1 a2))
  (define (make-product m1 m2) (list '* m1 m2))
  (define (make-exponentiation base exponent)
    (list '** base exponent)))
(define-unit constructor-alg-form-with-simplification@
  (import)
  (export constructors-algebraic-form^)
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))
  (define (make-exponentiation base exponent)
    (list '** base exponent)))

(define-unit derivation@
  (import constructors-algebraic-form^
          selectors-algebraic-form^
          predicates-algebraic-form^)
  (export derivation^)
  (define (deriv exp var)
    (cond ((number? exp) 0)
          ((variable? exp)
           (if (same-variable? exp var) 1 0))
          ((sum? exp)
           (make-sum (deriv (addend exp) var)
                     (deriv (augend exp) var)))
          ((product? exp)
           (make-sum
            (make-product (multiplier exp)
                          (deriv (multiplicand exp) var))
            (make-product (deriv (multiplier exp) var)
                          (multiplicand exp))))
          ((exponentiation? exp)
           (make-product (exponent exp)
                         (make-product (make-exponentiation (base exp)
                                                            (make-sum (exponent exp) -1))
                                       (deriv (base exp) var))))
          (else
           (error "unknown expression type -- DERIV" exp)))))

(module+ test
  (define (out . l)
    (map (lambda (x)
           (display x)
           (newline))
         l)
    '-----ok)
  ;; the selectors and the predicates are common for both tests.
  (define-values/invoke-unit/infer selectors-alg-form@)
  (define-values/invoke-unit/infer predicates-alg-form@)
  (define (test1)
    ;; the constructor are different
    (define-values/invoke-unit/infer constructor-alg-form-without-simplification@)
    (define-values/invoke-unit/infer derivation@)
    (out
     (deriv '(+ x 3) 'x)
     (deriv '(* x y) 'x)
     (deriv '(* (* x y) (+ x 3)) 'x)
     (deriv '(** (* x x) 2) 'x)))
  (define (test2)
    ;; the constructor are different
    (define-values/invoke-unit/infer constructor-alg-form-with-simplification@)
    (define-values/invoke-unit/infer derivation@)
    (out (deriv '(+ x 3) 'x)
         (deriv '(* x y) 'x)
         (deriv '(* (* x y) (+ x 3)) 'x)
         (deriv '(** (* x x) 2) 'x)))
  (test1)
  (test2) )

(module+ export
  (provide constructors-algebraic-form^
           selectors-algebraic-form^
           predicates-algebraic-form^
           derivation@
           selectors-alg-form@
           predicates-alg-form@
           constructor-alg-form-without-simplification@
           constructor-alg-form-with-simplification@))

