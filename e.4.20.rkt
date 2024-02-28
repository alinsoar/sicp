#lang racket

(require "sicp.rkt")
(GETMOD 4 1 without eval apply)
(GETMOD 4 6 let:)

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
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((letrec? exp) (eval (letrec->expand exp) env))
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

(define (letrec? exp) (tagged-list? exp 'letrec))
(define (letrec-var-bindings exp) (cadr exp))
(define (letrec-first-binding bindings) (car bindings))
(define (letrec-rest-binding bindings) (cdr bindings))
(define (letrec-binding-var binding) (car binding))
(define (letrec-binding-exp binding) (cadr binding))
(define (letrec-body exp) (cddr exp))
(define (letrec-apply-lambda vars body bindings)
  (cons (make-lambda vars body) bindings))

;;; INSTALL SCAN-OUT-DEFINES in LAMBDA-BODY
(define (letrec->expand exp)
  (let ((body (letrec-body exp))
        (vars (map letrec-binding-var
                   (letrec-var-bindings exp)))
        (vals (map letrec-binding-exp
                   (letrec-var-bindings exp))))
    (define (make-initial-assignments vars vals)
      (cond ((null? vars) '())
            (else
             (cons
              (cons 'set!
                    (list (car vars)
                          (car vals)))
              (make-initial-assignments (cdr vars) (cdr vals))))))
    (define initial-assignments (make-initial-assignments vars vals))
    (letrec-apply-lambda vars
                         (append initial-assignments body)
                         (map (lambda (_) ''*unassigned*) vars))))

(module+ test
  (define (test-eval e) (eval e the-global-environment))
  (define (test-eval-let e) (let:eval e let:the-global-environment))
  "LETREC"
  (letrec->expand
   '(letrec ((even?
              (lambda (n)
                (if (= n 0)
                    true
                    (odd? (- n 1)))))
             (odd?
              (lambda (n)
                (if (= n 0)
                    false
                    (even? (- n 1))))))
      (cons (odd? x)
            (cons (even? x)
                  '()))))
  (test-eval
   '(define (f x)
      (letrec ((even?
                (lambda (n)
                  (if (= n 0)
                      true
                      (odd? (- n 1)))))
               (odd?
                (lambda (n)
                  (if (= n 0)
                      false
                      (even? (- n 1))))))
        (cons (odd? x)
              (cons (even? x)
                    '())))))
  (test-eval
   '(cons (f 10)
          (cons (f 11)
                '())))
  (test-eval
   '(letrec ((fact
              (lambda (n)
                (if (= n 1)
                    1
                    (* n (fact (- n 1)))))))
      (fact 10)))
  "PRINT ENVIRONMENTS OF F"
  (test-eval
   '(define (f-with-print x)
      (letrec ((even?
                (lambda (n)
                  (print-environment 'env)
                  (if (= n 0)
                      true
                      (odd? (- n 1)))))
               (odd?
                (lambda (n)
                  (print-environment 'env)
                  (if (= n 0)
                      false
                      (even? (- n 1))))))
        (cons (odd? x)
              (cons (even? x)
                    '())))))
  (test-eval '(f-with-print 3))
  "TEST WITH LET INSTEAD OF LETREC"
  (test-eval-let
   '(define (f-with-print x)
      (let ((even?
             (lambda (n)
               (print-environment 'env)
               (if (= n 0)
                   true
                   (odd? (- n 1)))))
            (odd?
             (lambda (n)
               (print-environment 'env)
               (if (= n 0)
                   false
                   (even? (- n 1))))))
        (cons (odd? x)
              (cons (even? x)
                    '())))))
  (with-handlers (((lambda (v) (exn? v)) (lambda (v) (exn-message v))))
    (test-eval-let '(f-with-print 3)))
    'done)


