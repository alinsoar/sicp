#lang racket

(require "sicp.rkt")
(GETMOD 4 1 without
        eval apply
        lambda-body
        lookup-variable-value
        )
(GETMOD 4 6 without eval)

;;;SECTION 4.1.1
(define (eval exp env)
  '(d "~" exp)
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

;;; INSTALL SCAN-OUT-DEFINES in LAMBDA-BODY
(define (lambda-body exp)
  "Expand lambda when the lambda is defined."
  (let ((body (cddr exp)))
    (let ((lambda-data
           (scan-out-defines
            body
            (lambda (vars set-instr instructions)
              (if (eq? vars '())
                  body
                  (list
                   (let-apply-lambda
                    vars
                    (append set-instr instructions)
                    (map (lambda (_) ''*unassigned*) vars))))))))
      lambda-data)))
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (mcar vars))
             (if (eq? (mcar vals) '*unassigned*)
                 (error "Unassigned variable --" (mcar vars))
                 (mcar vals)))
            (else (scan (mcdr vars) (mcdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))
(define (scan-out-defines e co)
  (cond ((null? e) (co '() '() '()))
        ((definition? (car e))
         (scan-out-defines
          (cdr e)
          (lambda (vars set-instr instr)
            (co (cons (definition-variable (car e)) vars)
                (cons (cons 'set!
                            (list (definition-variable (car e))
                                  (definition-value (car e))))
                      set-instr)
                instr))))
        (else (scan-out-defines
               (cdr e)
               (lambda (vars set-instr instr)
                 (co vars set-instr (cons (car e) instr)))))))

(module+ test
  (define (test-eval e) (eval e the-global-environment))
  (test-eval '((lambda (test)
                 (define proc (lambda (test)
                                (print-environment 'env)
                                (+ test 2)))
                 (print-environment 'env)
                 (proc test))
               10))
  (test-eval '(((lambda (length)
                  (length length))
                (lambda (length)
                  (lambda (l)
                    (cond ((null? l) 0)
                          (else (+ 1 ((length length)
                                      (cdr l))))))))
               '(1 2 3)))
  (lambda-body '(lambda (test test0)
                  (define proc (lambda (test)
                                 (print-environment 'env)
                                 (+ test 2)))
                  '(print-environment 'env)
                  '(define test0 10)
                  (proc test)))
  (lambda-body '(lambda (f x)
                  (define (even? n)
                    (if (= n 0)
                        true
                        (odd? (- n 1))))
                  (define (odd? n)
                    (if (= n 0)
                        false
                        (even? (- n 1))))
                  (list (even? 1)
                        (even? 2)
                        (odd? 1)
                        (odd? 2))
                  (define test 111)
                  ))
  (test-eval '(begin
                (define (map op l)
                  (if (null? l)
                      '()
                      (cons (op (car l))
                            (map op (cdr l)))))
                ((lambda (x)
                   (map (lambda (x)
                          (cons (even? x) (odd? x)))
                        x)
                   (define (even? n)
                     (if (= n 0)
                         true
                         (odd? (- n 1))))
                   (define (odd? n)
                     (if (= n 0)
                         false
                         (even? (- n 1)))))
                 '(1 2 3 4 5))))
  (test-eval
   '((lambda (test)
       (define proc (lambda (test)
                      (print-environment 'env)
                      (+ test 2)))
       (print-environment 'env)
       (proc test))
     10))
  (d (with-handlers
         (((lambda (v) (exn? v)) (lambda (v) (exn-message v)))
          ((lambda (_) true) (lambda (_) (error "SHOULD NOT HAPPEN"))))
       (test-eval
        '((lambda (test)
            (define a b)
            (define b a)
            a)
          10))))
  'done)

(module+ export
  (provide eval
           apply
           scan-out-defines
           lookup-variable-value))

