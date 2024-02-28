#lang racket

(require "sicp.rkt")
(GETMOD 4 1 without
        eval apply
        lambda-body
        lookup-variable-value
        )
(GETMOD 1 44)
(GETMOD 4 6 without eval)

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
        ((delay? exp) (delay-expand exp))
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

;;; INSTALL SCAN-OUT-DEFINES in LAMBDA-BODY
(define (lambda-body exp)
  "Expand lambda when the lambda is defined."
  (define (make-new-unbounded-vars vars)
    (let ((N
           ;; N is the maximum length of any variable
           (reduce max
                   -1
                   (map (lambda (v) (string-length (symbol->string v)))
                        vars))))
      (map
       ;; convert from list of strings to list of symbols
       string->symbol
       (let ((name (make-string (+ 1 N) #\a)))
         (reduce (lambda (x y)
                   ;; make a list of (length vars) new variables of
                   ;; the form (AAA AAAA ...), the first one having a
                   ;; length N+1
                   (append (list (string-append "a" (car y)))
                           y))
                 (list name)
                 (cdr vars))))))
  (define (make-set-instr vars names)
    (if (null? vars)
        '()
        (cons (list 'set!
                    (car vars)
                    (car names))
              (make-set-instr (cdr vars) (cdr names)))))
  
  (let ((body (cddr exp)))
    (let ((lambda-data
           (scan-out-defines
            body
            (lambda (vars vals instructions)
              (if (eq? vars '())
                  body
                  (let ((names (make-new-unbounded-vars vars)))
                    (list
                     (let-apply-lambda
                      vars
                      (list
                        (let-apply-lambda
                         names
                         (append (make-set-instr vars names)
                                 instructions)
                         vals)
                        )
                      (map (lambda (_) ''*unassigned*) vars)))))))))
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
          (lambda (vars vals instr)
            (co (cons (definition-variable (car e)) vars)
                (cons (definition-value (car e))
                      vals)
                instr))))
        (else (scan-out-defines
               (cdr e)
               (lambda (vars vals instr)
                 (co vars vals (cons (car e) instr)))))))

(define (delay? exp) (tagged-list? exp 'delay))
(define (delay-expand exp) (lambda () exp))

(module+ test
  (define (test-eval e) (eval e the-global-environment))

  (test-eval '(define (stream-map f s) 'do-something))
  (test-eval '(define (integral d y0 dt) 'do-something))
  (test-eval '(define (solve f y0 dt)
                (error !!!)
                (print-environment 'env) ; This is never reached
                (define y (integral (delay dy) y0 dt))
                (define dy (stream-map f y))
                y))

  "-- LOOKUP-VARIABLE-VALUE GENERATES THE ERROR CORRESPONDING TO *unassigned* VALUE"
  (with-handlers
      (((lambda (v) (exn? v)) (lambda (v) (exn-message v)))
       ((lambda (_) true) (lambda (_) (error "SHOULD NOT HAPPEN"))))
    (test-eval '(solve
                 (print-environment 'env)
                 0
                 0)))
  
  (lambda-body '(lambda (f y0 dt)
                  (define y (integral (delay dy) y0 dt))
                  (define dy (stream-map f y))
                  y))
  'done)


