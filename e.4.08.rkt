#lang racket

(require "sicp.rkt")
(GETMOD 4 1 without eval apply)
(GETMOD 4 6 without eval)

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
        ((let-function? exp)
         (let ((let-defun (let->function-definition exp)))
           (let ((ee (extend-environment (list) (list) env)))
             (let ((proc (eval (car let-defun) ee))
                   (bindings (cdr let-defun)))
               (define-variable! (let-defun-name exp) proc ee)
               (print-environment ee)
               (eval let-defun ee)))))
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

(define (let-function? exp) (and (tagged-list? exp 'let)
                                 (symbol? (let-var-bindings exp))))

(define (let->function-definition exp)
  (define (iter bindings co)
    (cond ((null? bindings)
           (co '() '()))
          (else
           (let ((first (let-defun-first-binding bindings))
                 (rest  (let-defun-rest-binding bindings)))
             (iter rest
                   (lambda (vars expr)
                     (co (cons (let-defun-binding-var first) vars)
                         (cons (let-defun-binding-exp first) expr))))))))
  (iter (let-defun-var-bindings exp)
        (lambda (v e) (cons (make-lambda v (let-defun-body exp))
                            e))))

(define (let-defun-var-bindings exp) (caddr exp))
(define (let-defun-name exp) (cadr exp))
(define (let-defun-first-binding bindings) (car bindings))
(define (let-defun-rest-binding bindings) (cdr bindings))
(define (let-defun-binding-var binding) (car binding))
(define (let-defun-binding-exp binding) (cadr binding))
(define (let-defun-body exp) (cdddr exp))

(module+ test
  (define (test-eval e) (eval e the-global-environment))

  (let->combination '(let ((v 10) (w 20)) (+ v w)))
  (test-eval '(let ((v 10)) v))
  (test-eval '(let ((v 10) (w 20)) (+ v w)))
  (let->function-definition
   '(let fib-iter ((a 1)
                   (b 0)
                   (count n))
      (if (= count 0)
          b
          (fib-iter (+ a b) a (- count 1)))))
  "---"
  (test-eval
   '(begin (define (fib n)
             (let fib-iter ((a 1)
                            (b 0)
                            (count n))
               (if (= count 0)
                   b
                   (fib-iter (+ a b) a (- count 1)))))
           (fib 10)))
  'done)

