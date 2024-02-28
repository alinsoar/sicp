#lang racket

(require "sicp.rkt")
(GETMOD 4 1 without
        eval apply
        application? operator operands no-operands? first-operand rest-operands )

(define-signature application-data^
  (application? operator operands no-operands? first-operand rest-operands))
(define-signature eval^ (eval))

(define-unit EVAL-APPLY@
  (import application-data^)
  (export eval^)
  (define (eval exp env)
    (cond ((self-evaluating? exp) exp)
          ((application? exp)
           (apply (eval (operator exp) env)
                  (list-of-values (operands exp) env)))
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
          (else
           (error "Unknown expression type -- EVAL" exp))))
  (define (apply procedure arguments)
    (cond ((primitive-procedure? procedure)
           (apply-primitive-procedure procedure arguments))
          ((compound-procedure? procedure)
           (eval-sequence
            (procedure-body procedure)
            (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
          (else
           (error
            "Unknown procedure type -- APPLY" procedure))))

  (define (list-of-values exps env)
    (if (no-operands? exps)
        '()
        (cons (eval (first-operand exps) env)
              (list-of-values (rest-operands exps) env))))
  (define (eval-if exp env)
    (if (true? (eval (if-predicate exp) env))
        (eval (if-consequent exp) env)
        (eval (if-alternative exp) env)))
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
    'ok))

(define-unit APPLICATION-DATA-WITH-CALL@
  (import)
  (export application-data^)
  (define (application? exp) (tagged-list? exp 'call))
  (define (operator exp) (cadr exp))
  (define (operands exp) (cddr exp))
  (define (no-operands? ops) (null? ops))
  (define (first-operand ops) (car ops))
  (define (rest-operands ops) (cdr ops)))

(define-unit APPLICATION-DATA@
  (import)
  (export application-data^)
  (define (application? exp) (pair? exp))
  (define (operator exp) (car exp))
  (define (operands exp) (cdr exp))
  (define (no-operands? ops) (null? ops))
  (define (first-operand ops) (car ops))
  (define (rest-operands ops) (cdr ops)))

(module+ test
  (require rackunit)

  (define (test1)
    (define-values/invoke-unit/infer APPLICATION-DATA@)
    (define-values/invoke-unit/infer EVAL-APPLY@)
    (define (test-eval e) (eval e the-global-environment))
    (d (with-handlers (((lambda (v) (exn? v)) (lambda (v) (exn-message v)))
                       ((lambda (_) true) (lambda (_) (error "SHOULD NOT HAPPEN"))))
         (test-eval '(define x 3)))))
  (define (test2)
    (define-values/invoke-unit/infer APPLICATION-DATA-WITH-CALL@)
    (define-values/invoke-unit/infer EVAL-APPLY@)
    (define (test-eval e) (eval e the-global-environment))
    (test-eval '(call cons (call (lambda (x) (call + x x))
                                 3)
                      (call + 1 2 3))))
  (test1)
  (test2)
  'done)

