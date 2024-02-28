#lang racket

(require "sicp.rkt")
(GETMOD 4 1 without eval apply)

;;;SECTION 4.1.1
(define (eval exp env)
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
        ((for? exp) (eval (for-expand exp) env))
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

(define (for?            exp) (and (tagged-list? exp 'for)
                                   (pair? (cdr exp))
                                   (symbol? (cadr exp))
                                   (pair? (cdr (cdr exp)))
                                   (number? (caddr exp))
                                   (pair? (cddr (cdr exp)))
                                   (number? (cadddr exp))
                                   (pair? (cdddr (cdr exp)))
                                   (number? (car (cddddr exp)))
                                   (tagged-list? (cddddr (cdr exp)) 'repeat)
                                   ))
(define (for-name        exp) (car (cdr exp)))
(define (for-range-start exp) (cadr (cdr exp)))
(define (for-range-end   exp) (caddr (cdr exp)))
(define (for-range-step  exp) (cadddr (cdr exp)))
(define (for-body        exp) (cddddr (cddr exp)))

(define (for-expand exp)
  `(((lambda (loop)
       (loop loop))
     (lambda (loop)
       (lambda (,(for-name exp))
         (cond ,(cons `(= ,(for-name exp) ,(for-range-end exp))
                      (for-body exp))
               (else
                ,(cons 'begin
                       (append (for-body exp)
                               `(((loop loop)
                                  (+ ,(for-name exp)
                                     ,(for-range-step exp)))))))))))
    ,(for-range-start exp)))

(module+ test
  (define (test-eval e) (eval e the-global-environment))
  "-- FOR WITH MODIFIED SYNTAX; no change in EVAL/APPLY --"
  (test-eval '(begin
                (define x 0)
                ;; for NAME start end step REPEAT body
                (for a 1 10 1 repeat
                     (set! x (+ x a))
                     x)))
  (test-eval '(begin
                "this is factorial"
                (define x 1)
                (for n 100 0 -1 repeat
                     (if (= n 0)
                         'ok
                         (set! x (* x n)))
                     x))))

