#lang racket

(require "sicp.rkt")
(GETMOD 4 1 without eval apply)

;;;SECTION 4.1.1
(define (eval exp env)
  '(d "~" exp)
  (and (eq? exp 'y) (print-environment env))
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((get-environment? exp) env)
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((unless? exp) (eval-unless exp env))
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
(define (eval-unless exp env)
  (eval
   (if (true? (eval (unless-condition exp) env))
       (unless-exceptional-value exp)
       (unless-usual-value exp))
   env))
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

;;; UNLESS AS SYNTAX
(define (unless? exp) (tagged-list? exp 'unless))
(define (unless-condition exp) (cadr exp))
(define (unless-usual-value exp) (caddr exp))
(define (unless-exceptional-value exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(module+ test
  (require rackunit)
  (require racket/sandbox)
  (define (test-eval e) (eval e the-global-environment))
  (test-eval '(define (factorial n)
               (if (0? (% n 1001)) (print n "..."))
               (unless (= n 1)
                 (* n (factorial (- n 1)))
                 1)))

  "Factorial: unless as syntax"
  (test-eval '(factorial 3))
  (test-eval '(factorial 5))
  (test-eval '(factorial 10))
  (test-eval '(factorial 100))

  (test-eval
   '(define test0
      (lambda (a) (lambda (b) (unless (= b 0)
                                (/ a b)
                                (print "error -- " a "/" b "\n"))))))
  (test-eval '((test0 0) 1))
  (test-eval '((test0 0) 0))

  "Alyssa cannot use a variable pointing UNLESS to define some h.o.f."
  (with-handlers (((lambda (v) (exn? v)) (lambda (v) (exn-message v))))
    (test-eval '(define test1 unless)))
  (with-handlers (((lambda (v) (exn? v)) (lambda (v) (exn-message v))))
    (test-eval 'test1))
  (with-handlers (((lambda (v) (exn? v)) (lambda (v) (exn-message v))))
    (test-eval 'unless))

  'done)

