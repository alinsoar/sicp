#lang racket

(require "sicp.rkt")
(require scheme/mpair)
(require compatibility/mlist)
(GETMOD 4 1 without
        eval apply
        lookup-variable-value
        enclosing-environment
        first-frame
        frame-variables
        frame-values
        the-empty-environment
        extend-environment
        set-variable-value!
        define-variable!
        print-environment
        the-global-environment
        add-binding-to-frame!)

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
        ((makunbound? exp)
         (let ((new-frame (eval-makunbound exp env)))
           (set-mcar! env new-frame)))
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
      (mcons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((leftmost-value (eval (first-operand exps) env)))
        (mcons leftmost-value (list-of-values (rest-operands exps) env)))))
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

;;; EVERYTHING IS REDEFINED AS MUTABLE INSIDE THE ENVIRONMENT
(define (enclosing-environment env) (mcdr env))
(define (first-frame env) (mcar env))
(define (make-frame variables values)
  (mcons variables values))
(define (frame-variables frame) (mcar frame))
(define (frame-values frame) (mcdr frame))
(define the-empty-environment (mlist))
(define (add-binding-to-frame! var val frame)
  (set-mcar! frame (mcons var (mcar frame)))
  (set-mcdr! frame (mcons val (mcdr frame))))
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (mcons (make-frame (list->mlist vars) (list->mlist vals)) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (mcar vars))
             (mcar vals))
            (else (scan (mcdr vars) (mcdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (mcar vars))
             (set-mcar! vals val))
            (else (scan (mcdr vars) (mcdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))
(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (mcar vars))
             (set-mcar! vals val))
            (else (scan (mcdr vars) (mcdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))
(define (print-environment env)
  (define (iter env N)
    (let ((ee (enclosing-environment env)))
      (rp (* N 4) " ")
      (cond ((eq? ee the-empty-environment) (d "GLOBAL ENVIRONMENT"))
            ((> N 0) (d "PARENT ENVIRONMENT"))
            (else (d "ENVIRONMENT")))
      (define (iter-frame var exp)
        (if (null? var)
            'ok
            (begin (rp (* 4 N) " ")
                   (d (mcar exp) "=>"
                      (cond ((compound-procedure? (mcar var))
                             (format "compound-procedure ~a ~a ~a"
                                     (procedure-parameters (mcar var))
                                     (procedure-body (mcar var))
                                     (frame-variables
                                      (first-frame
                                       (procedure-environment
                                        (mcar var))))))
                            (else (mcar var))))
                   (iter-frame (mcdr var) (mcdr exp)))))
      (iter-frame
       (frame-values (first-frame env))
       (frame-variables (first-frame env)))
      (if (eq? ee the-empty-environment)
          'ok
          (iter ee (+ N 1)))))
  (iter env 0))

;;; LINK THE NEW DEFS
(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))
(define the-global-environment (setup-environment))
(define (set-the-global-environment! env) (set! the-global-environment env))

;;; MAKE-UNBOUND!
(define (remove-from-frame frame var)
  (define (scan vars vals)
    (cond ((null? vars) (error "name" var "not found."))
          ((eq? var (mcar vars))
           (mcons (mcdr vars)
                  (mcdr vals)))
          (else (let ((n (scan (mcdr vars) (mcdr vals))))
                  (mcons (mcons (mcar vars) (mcar n))
                         (mcons (mcar vals) (mcdr n)))))))
  (scan (frame-variables frame)
        (frame-values frame)))

(define (makunbound? exp) (tagged-list? exp 'make-unbound!))
(define makunbound-name cadr)
(define (eval-makunbound exp env)
  (remove-from-frame (first-frame env)
                     (eval (makunbound-name exp) env)))

(module+ test
  (require rackunit)
  (define (test-eval e) (eval e the-global-environment))
  (test-case
   "remove a variable from a frame"
   (check-exn
    exn:fail?
    (lambda ()
      (test-eval '(begin
                    (define x 10)
                    (make-unbound! 'x)
                    x))
      'no-error-caught!))
   "Unbound variable ``x``")
  (test-eval '(begin (define x 10)
                     (make-unbound! 'x)
                     (define x "now ``x`` is defined again")
                     x))
  'done)



