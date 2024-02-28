#lang racket

(require "sicp.rkt")
(GETMOD 4 1 without
        eval apply
        lookup-variable-value
        make-procedure)
(GETMOD 4 1 sequential:)
(GETMOD 4 16 lambda-expansion:)
(GETMOD 1 44)

;;;SECTION 4.1.1
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lambda-expansion:lookup-variable-value exp env))
        ((get-environment? exp) env)
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
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
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

(define (list-of-values-ORIG exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((leftmost-value (eval (first-operand exps) env)))
        (cons leftmost-value (list-of-values (rest-operands exps) env)))))
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
  'ok)

(define (make-procedure parameters body env)
  "move the definitions at the beginning"
  (define (test-parameters-and-vars? vars)
    (map car
         (filter
          (lambda (x) (eq? (car x) (cdr x)))
          (reduce append '()
                  (map (lambda (x)
                         (map (lambda (y) (cons x y))
                              parameters))
                       vars)))))
  (let ((lambda-data
         (scan-out-defines
          body
          (lambda (vars set-instr instructions)
            'ok
            (cond ((eq? vars '()) body)
                  ((not (eq? '() (test-parameters-and-vars? vars)))
                   (error "variable/s" (test-parameters-and-vars? vars)
                          "already defined -- LAMBDA EXPAND"))
                  (else
                   'ok
                   '(print-environment env)
                   (append set-instr instructions)))))))
    (list 'procedure parameters lambda-data env)))
(define (scan-out-defines e co)
  "return a list of defines and a list of the other instructions."
  (cond ((null? e) (co '() '() '()))
        ((definition? (car e))
         (scan-out-defines
          (cdr e)
          (lambda (vars set-instr instr)
            (co (cons (definition-variable (car e)) vars)
                (cons (car e) set-instr)
                instr))))
        (else (scan-out-defines
               (cdr e)
               (lambda (vars set-instr instr)
                 (co vars set-instr (cons (car e) instr)))))))

(module+ test
  (define (test-eval-sequential e)
    (sequential:eval e the-global-environment))
  (define (test-eval-lambda-expansion e)
    (lambda-expansion:eval e the-global-environment))
  (define (test-eval-simultaneous-lambda-apply e)
    (eval e the-global-environment))
  (define (test evaluator)
    (evaluator '((lambda (test test0)
                   (define proc (lambda (test2)
                                  (print-environment 'env)
                                  (+ test test2 2)))
                   (print-environment 'env)
                   (proc test1)
                   (define test1 10))
                 111 '())))
  "-- SEQUENTIAL --"
  (with-handlers
      (((lambda (v) (exn? v)) (lambda (v) (exn-message v)))
       ((lambda (_) true) (lambda (_) (error "SHOULD NOT HAPPEN"))))
    (test test-eval-sequential))
  "-- LAMBDA EXPANSION -- A NEW ENVIRONMENT WITH ``PROC`` IS CREATED"
  (test test-eval-lambda-expansion)
  "-- SIMULTANEOUS LAMBDA APPLY -- DEFINITIONS MOVED FORWARD"
  (test test-eval-simultaneous-lambda-apply)
  'done)

