#lang racket

(require "sicp.rkt")
(GETMOD 4 1 without eval apply)

(define (eval exp env)
  (define (get-call op)
    (define (quoted) (text-of-quotation exp))
    (define (assignment) (eval-assignment exp env))
    (define (definition) (eval-definition exp env))
    (define (if-conditional) (eval-if exp env))
    (define (make-proc) (make-procedure (lambda-parameters exp)
                                        (lambda-body exp)
                                        env))
    (define (sequence) (eval-sequence (begin-actions exp) env))
    (define (conditional) (eval (cond->if exp) env))
    (define (application) (apply (eval (operator exp) env)
                                 (list-of-values (operands exp) env)))
    (cond ((eq? op 'quote) quoted)
          ((eq? op 'set!) assignment)
          ((eq? op 'define) definition)
          ((eq? op 'if) if-conditional)
          ((eq? op 'lambda) make-proc)
          ((eq? op 'begin) sequence)
          ((eq? op 'cond) conditional)
          (else application)))
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((pair? exp) ((get-call (operator exp))))
        (else (error "Unknown expression type -- EVAL" exp))))

;;; The rest will link with this new eval
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

(module+ test
  (define (test-eval e) (eval e the-global-environment))
  (test-eval '(+ 1 2 3))
  (test-eval '(if (< 3 5) 1 2))
  (test-eval '(if (< 5 3) 1 2))
  (test-eval '(if (< 5 3) 'false (cons 'a (cons (+ 3 5) '()))) )
  (test-eval '(if (< 3 5) 'true 'false) )
  'done)
