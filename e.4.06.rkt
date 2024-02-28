#lang racket

(require "sicp.rkt")
(GETMOD 4 1 without
        eval apply)

;;;SECTION 4.1.1
(define (eval exp env)
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
        ((let? exp) (eval (let->combination exp) env))
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

(define (let? exp) (and (tagged-list? exp 'let)
                        (pair? (let-var-bindings exp))))
(define (let->combination exp)
  (define (iter bindings co)
    (cond ((null? bindings)
           (co '() '()))
          (else
           (let ((first (let-first-binding bindings))
                  (rest  (let-rest-binding bindings)))
              (iter rest
                    (lambda (vars expr)
                      (co (cons (let-binding-var first) vars)
                          (cons (let-binding-exp first) expr))))))))
  (iter (let-var-bindings exp)
        (lambda (v e) (let-apply-lambda v (let-body exp) e))))

(define (let-var-bindings exp) (cadr exp))
(define (let-first-binding bindings) (car bindings))
(define (let-rest-binding bindings) (cdr bindings))
(define (let-binding-var binding) (car binding))
(define (let-binding-exp binding) (cadr binding))
(define (let-body exp) (cddr exp))
(define (make-let bindings body)
  (list (cons 'let (cons (list bindings) body))))
(define (let-apply-lambda vars body bindings)
  (cons (make-lambda vars body) bindings))

(module+ test
  (define (test-eval e) (eval e the-global-environment))
  (let->combination '(let ((v 10) (w 20)) (+ v w)))
  "---"
  (test-eval '(let ((v 10)) v))
  (test-eval '(let ((v 10) (w 20)) (+ v w)))
  (test-eval '((lambda (x y)
                 (let ((v (* 2 x)) (w (* 2 y)))
                   (let ((a v) (b w))
                     (print-environment 'env)
                     (+ a b v w))))
               10 20))
  'done)

(module+ export
  (provide let-var-bindings
           let-first-binding
           let-rest-binding
           let-binding-var
           let-binding-exp
           let-body
           let?
           make-let
           let->combination
           let-apply-lambda
           eval
           the-global-environment
           ))

