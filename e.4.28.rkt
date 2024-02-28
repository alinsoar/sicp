#lang racket

(require "sicp.rkt")
(GETMOD 4 1 without eval apply)
(GETMOD 4 27 without
        actual-value
        evaluated-thunk?
        thunk-value
        )

;;; Modifying the evaluator
(define (eval exp env)
  (d "~" exp)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)             ; clause from book
         (apply (operator exp)
                (operands exp)
                env))
        (else
         (error "Unknown expression type -- EVAL" exp))))
(define (actual-value exp env)
  (force-it (eval exp env)))
(define (apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env))) ; changed
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-delayed-args arguments env) ; changed
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((leftmost-value (eval (first-operand exps) env)))
        (cons leftmost-value (list-of-values (rest-operands exps) env)))))
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)
(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps)
                                env))))
(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps) env)
            (list-of-delayed-args (rest-operands exps)
                                  env))))
(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

;;; Representing thunks

;; "thunk" that has been forced and is storing its (memoized) value
(define (evaluated-thunk? obj) (mtagged-list? obj 'evaluated-thunk))
(define (thunk-value evaluated-thunk) (mcar (mcdr evaluated-thunk)))

;; memoizing version of force-it
(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value
                        (thunk-exp obj)
                        (thunk-env obj))))
           (set-mcar!       obj  'evaluated-thunk)
           (set-mcar! (mcdr obj) result) ; replace exp with its value
           (set-mcdr! (mcdr obj) '())    ; forget unneeded env
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))

(module+ test
  (define (test-lazy-eval e) (actual-value e the-global-environment))
  (test-lazy-eval '+)

  "APPLY NEEDS TO DISPATCH IN FUNCTION OF THE TYPE OF THE OPERATOR"
  (with-handlers (((lambda (v) (exn? v)) (lambda (v) (exn-message v))))
    (test-lazy-eval '(+ 1 2 3)))
  
  (test-lazy-eval '(define (abs x)
                     ((if (< 0 x)
                          +
                          -)
                      x)))
  (with-handlers (((lambda (v) (exn? v)) (lambda (v) (exn-message v))))
    (test-lazy-eval '(abs -10)))
  'done)

