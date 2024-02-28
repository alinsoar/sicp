#lang racket

(require "sicp.rkt")
(require (submod "e.1.44.rkt" export))
(require racket/trace)
(define (atom? x) (or (null? x)
                      (not (list? x))))

;;; Skeleton code from the Pattern Matcher of the article
;;; -- Lisp: a Language for Stratified Design --
;;; Pattern Matcher and Simplification

(define (make-dictionary pattern expression dictionary)
  (define headers
    (lambda (head tail)
      (if (null? tail)
          (list (cons head tail))
          (cons (cons head tail)
                (headers (append head (list (car tail)))
                         (cdr tail))))))
  
  (cond ((eq? dictionary 'failed) 'failed)
        ((atom? pattern)
         (if (atom? expression)
             (if (eq? pattern expression)
                 dictionary
                 'failed)
             'failed))
        ((KLEENE? pattern)
         (extend-dictionary pattern expression dictionary))
        ((CONSTANT? pattern)
         (if (constant? expression)
             (extend-dictionary pattern expression dictionary)
             'failed))
        ((VARIABLE? pattern)
         (if (variable? expression)
             (extend-dictionary pattern expression dictionary)
             'failed))
        ((EXPRESSION? pattern)
         (extend-dictionary pattern expression dictionary))
        ((HEAD-KLEENE? pattern)
         (define loop
           (lambda (exp-list)
             (if (null? exp-list)
                 'failed
                 (let* ((exp (car exp-list))
                        (head-exp (car exp))
                        (tail-exp (cdr exp))
                        (dictionary
                         (make-dictionary (cdr pattern)
                                          tail-exp
                                          (make-dictionary (car pattern)
                                                           head-exp
                                                           dictionary))))
                   (if (eq? dictionary 'failed)
                       (loop (cdr exp-list))
                       dictionary)))))
         (if (or (null? expression) (pair? expression))
             (loop (headers '() expression))
             'failed))
        ((atom? expression) 'failed)
        (else
         (make-dictionary (cdr pattern)
                          (cdr expression)
                          (make-dictionary (car pattern)
                                           (car expression)
                                           dictionary)))))
(define (instantiate skeleton dictionary)
  (define cons-in-tail
    (lambda (l co)
      (if (null? (cdr l))
          (co (car l))
          (cons-in-tail (cdr l)
                        (lambda (x)
               (co (cons (car l) x)))))))
  (cond ((atom? skeleton) skeleton)
        ((skeleton-evaluation? skeleton)
         (evaluate (evaluation-expression skeleton)
                       dictionary))
        ((skeleton-k? skeleton)
         (cons-in-tail (map (lambda (s) (instantiate s dictionary))
                            (cdr skeleton))
                       (lambda (x) x)))
        (else (cons (instantiate (car skeleton) dictionary)
                    (instantiate (cdr skeleton) dictionary)))))
(define (simplifier the-rules)
  (define (simplify-exp exp)
    (try-rules (if (compound? exp)
                   (simplify-parts exp)
                   exp)))
  (define (simplify-parts exp)
    (if (null? exp)
        '()
        (cons (simplify-exp   (car exp))
              (simplify-parts (cdr exp)))))
  (define (try-rules exp)
    (define (scan rules)
      (if (null? rules)
          exp
          (let* ((head-rule (car rules))
                 (tail-rules (cdr rules))
                 (pattern (pattern head-rule)))
            (let ((dictionary (make-dictionary pattern
                                               exp
                                               (make-empty-dictionary))))
              (if (eq? 'failed dictionary)
                  (scan tail-rules)
                  (simplify-exp (instantiate (skeleton head-rule) dictionary)))))))
    '(trace scan)
    (scan the-rules))
  '(trace try-rules)
  '(trace simplify-exp)
  '(trace simplify-parts)
  simplify-exp)

;; Dictionaries
(define (make-empty-dictionary) '())
(define (extend-dictionary pat dat dictionary)
  (let ((vname (VARIABLE-NAME pat)))
    (let ((v (assq vname dictionary)))
      (cond ((false? v)
             (cons (list vname dat) dictionary))
            ((eq? (cadr v) dat) dictionary)
            (else 'failed)))))
(define (lookup var dictionary)
  (let ((v (assq var dictionary)))
    (if (false? v)
        var
        (cadr v))))

;; Expressions
(define (compound? exp) (pair?   exp))
(define (constant? exp) (number? exp))
(define (variable? exp) (atom?   exp))

;; Rules
(define (pattern  rule) (car  rule))
(define (skeleton rule) (cadr rule))

;; Patterns
(define (CONSTANT?     pattern)
  (if (pair? pattern) (eq? (car pattern) '?c) false))
(define (EXPRESSION?   pattern)
  (if (pair? pattern) (eq? (car pattern) '? ) false))
(define (VARIABLE?     pattern)
  (if (pair? pattern) (eq? (car pattern) '?v) false))
(define (KLEENE?       pattern)
  (if (pair? pattern)
      (eq? (car pattern) '?*)
      false))
(define (HEAD-KLEENE?  pattern)
  (if (and (pair? pattern)
           (pair? (car pattern)))
      (eq? (caar pattern) '?*)
      false))
(define (VARIABLE-NAME pattern)
  (cadr pattern))

;; Skeletons & Evaluations
(define (skeleton-k?    skeleton)
  (if (pair? skeleton) (eq? (car skeleton) '@) false))
(define (skeleton-evaluation?    skeleton)
  (if (pair? skeleton) (eq? (car skeleton) ':) false))
(define (evaluation-expression evaluation) (cadr evaluation))

(define user-initial-environment (make-base-namespace))

;; Evaluate (dangerous magic)
(define (evaluate form dictionary)
  (if (atom? form)
      (lookup form dictionary)
      ;; (apply (EVAL (lookup (car form) dictionary)
      ;;              user-initial-environment)
      ;;        (mapcar (lambda (v) (lookup v dictionary))
      ;;                (cdr form)))
      (eval (cons (lookup (car form) dictionary)
                  (map (lambda (v) (lookup v dictionary))
                       (cdr form)))
            user-initial-environment)))

;; A couple sample rule databases...
;; Algebraic simplification
(define algebra-rules
  '(
    ( ((? op) (?c c1) (?c c2))                (: (op c1 c2))                )
    ( (+ (?  e ) (?c c ))                     (+ (: c) (: e))               )
    ( (* (?  e ) (?c c ))                     (* (: c) (: e))               )
    ( (+ 0 (? e))                             (: e)                         )
    ( (* 1 (? e))                             (: e)                         )
    ( (* 0 (? e))                             0                             )
    ( (* (?c c1) (* (?c c2) (? e )))          (* (: (* c1 c2)) (: e))       )
    ( (* (?  e1) (* (?c c ) (? e2)))          (* (: c ) (* (: e1) (: e2)))  )
    ( (* (* (? e1) (? e2)) (? e3))            (* (: e1) (* (: e2) (: e3)))  )
    ( (+ (?c c1) (+ (?c c2) (? e )))          (+ (: (+ c1 c2)) (: e))       )
    ( (+ (?  e1) (+ (?c c ) (? e2)))          (+ (: c ) (+ (: e1) (: e2)))  )
    ( (+ (+ (? e1) (? e2)) (? e3))            (+ (: e1) (+ (: e2) (: e3)))  )
    ( (+ (* (?c c1) (? e)) (* (?c c2) (? e))) (* (: (+ c1 c2)) (: e))       )
    ( (* (? e1) (+ (? e2) (? e3)))            (+ (* (: e1) (: e2))
                                                 (* (: e1) (: e3)))         )
    ;; distribution of multiplication over addition
    ( (* (? e1) (+ (? e2) (? e3)))            (+ (* (: e1) (: e2))
                                                 (* (: e1) (: e3)))         )
    ( (* (+ (? e2) (? e3)) (? e1))            (+ (* (: e1) (: e2))
                                                 (* (: e1) (: e3)))         )
    ;; abstractions
    ( (fib-pat 1)                             (fib-pat 0)                   )
    ( (fib-pat 0)                             1                             )
    ( (fib-pat (? n))                        (+ '(fib-pat (- (: n) 2))
                                                '(fib-pat (- (: n) 1)))     )
    ( (fib-lazy 1)                            (fib-lazy 0)                  )
    ( (fib-lazy 0)                            1                             )
    ( (fib-lazy (? n))                        '(+ (fib (- (: n) 2))
                                                  (fib-lazy (- (: n) 1)))   )
    ( (fib 1)                                 (fib 0)                       )
    ( (fib 0)                                 1                             )
    ( (fib (? n))                             (+ (fib (- (: n) 2))
                                                 (fib (- (: n) 1)))         )
    ( (fact-lazy 0)                           1                             )
    ( (fact-lazy (? n))                       (* (fact-lazy (- (: n) 1))
                                                 '(: n))                    )
    ( (fact 0)                                1                             )
    ( (fact (? n))                            (* (fact (- (: n) 1))
                                                 (: n))                     )
    ))
(define algsimp (simplifier algebra-rules))

;; Symbolic Differentiation
(define deriv-rules
  '(
    ( (deriv (?c c) (? v))              0                                    )
    ( (deriv (?v v) (? v))              1                                    )
    ( (deriv (?v u) (? v))              0                                    )
    ( (deriv (+ (? x1) (? x2)) (? v))   (+ (deriv (: x1) (: v))
                                           (deriv (: x2) (: v)))             )
    ( (deriv (* (? x1) (? x2)) (? v))   (+ (* (: x1) (deriv (: x2) (: v)))
                                           (* (deriv (: x1) (: v)) (: x2)))  )
    ( (deriv (** (? x) (?c n)) (? v))   (* (* (: n) (** (: x) (- (: n) 1)))
                                           (deriv (: x) (: v)))              )
    ))
(define dsimp (simplifier deriv-rules))

;;; Kleene star reduction test
(define test-rules
  '(
    ;; prioritize multiplication
    (((?* h) * (?* t)) (M (: h) (: t)))
    ;; leftmost
    (((? e1) (? op) (? e2) (? op) (?* t)) (((: e1) (: op) ((: e2) (: op) (: t)))))
    ;; rightmost
    (((? e1) (? op) (? e2) (? op) (?* t)) (((: e1) (: op) (: e2)) (: op) (: t)))
    ))
(define tests (simplifier test-rules))

(module+ test
  (tests '(2 - 4 + 5))
  (tests '(2 - 4 * 9 + 5 * 7 + 3 + 1 + 4))
  
  (dsimp '(deriv 10 x))
  (dsimp '(deriv x x))
  (dsimp '(deriv y x))
  "---"
  (algsimp (dsimp '(deriv (** x 3) x)))
  (algsimp (dsimp '(deriv (** (+ x (+ y (* 3 x))) 11) x)))
  "---"
  (dsimp '(deriv (* (+ x z) (+ x (* x y))) x))
  (algsimp (dsimp '(deriv (* (+ x z) (+ x (* x y))) x)))
  (algsimp (algsimp (dsimp '(deriv (* (+ x z) (+ x (* x y))) x))))
  (algsimp '(fib 15))
  (algsimp '(fib-lazy 15))
  (algsimp '(fib-pat 6))
  (algsimp '(fact 15))
  (algsimp '(fact-lazy 15))
  'done)

(module+ export
  (provide algsimp
           dsimp
           algebra-rules
           simplifier
           ))

