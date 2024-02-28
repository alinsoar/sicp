#lang racket

(require "sicp.rkt")
(GETMOD 4 6 without eval)
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
        ((let? exp) (eval (let->combination exp) env))
        ((let*? exp) (eval (let*->combination exp) env))
        ((LET*? exp) (eval (LET*->nested-lets exp) env))
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

(define (let*? exp) (tagged-list? exp 'let*))
(define (LET*? exp) (tagged-list? exp 'LET*))

(define (let*->combination exp)
  "rewrite let* as combinations of lambdas"
  (define (iter bindings co)
    (cond ((null? bindings)
           (co (let-body exp)))
          (else
           (let ((first (let-first-binding bindings))
                 (rest  (let-rest-binding bindings)))
             (iter rest
                   (lambda (x)
                     (define (make-application e b) (list (list e b)))
                     (co
                      (list (let-apply-lambda
                             (list (let-binding-var first))
                             x
                             (list (let-binding-exp first)))))))))))
  (iter (let-var-bindings exp)
        (lambda (x) (car x))))

(define (LET*->nested-lets exp)
  "rewrite LET* as combinations of let"
  (define (iter bindings co)
    (cond ((null? bindings)
           (co (let-body exp)))
          (else
           (let ((first (let-first-binding bindings))
                 (rest  (let-rest-binding bindings)))
             (iter rest
                   (lambda (x)
                     (co (make-let first x))))))))
  (iter (let-var-bindings exp)
        (lambda (x) (car x))))

(module+ test
  (define (test-eval e) (eval e the-global-environment))
  (let*->combination '(let* ((v 10) (w (+ v 1)))
                        10
                        (+ v w)))
  "---"
  (test-eval '(let* ((v 10)) v))
  (test-eval '(let* ((v 10) (w 20)) (+ v w)))
  (test-eval '(let* ((v 10) (w (+ v 1))) (+ v w)))

  (test-eval ' (let* ((x 3)
                      (y (+ x 2))
                      (z (+ x y 5)))
                 (* x z)))

  "---"

  (LET*->nested-lets '(let* ((v 10) (w (+ v 1)))
                        10
                        (cons v w)))
  (test-eval '(LET* ((v 10) (w (+ v 1)))
                    (cons v w)))
  (test-eval ' (LET* ((x 3)
                      (y (+ x 2))
                      (z (+ x y 5)))
                 (* x z)))

  (test-eval (LET*->nested-lets '(LET* ((x 3)
                                        (y (+ x 2))
                                        (z (+ x y 5)))
                                       (* x z))))
  'done)
