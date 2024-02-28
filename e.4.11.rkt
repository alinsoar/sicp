#lang racket

(require "sicp.rkt")
(require scheme/mpair)
(require compatibility/mlist)
(GETMOD 4 1 without
        eval apply
        extend-environment
        lookup-variable-value
        set-variable-value!
        define-variable!
        print-environment
        the-global-environment
        the-empty-environment
        enclosing-environment
        first-frame
        frame-values
        frame-variables
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

;;; NEW DEF OF A FRAME
(define (mzip l1 l2) (mmap mcons l1 l2))
(define (make-frame variables values)
  (mzip variables values))
(define (frame-variables frame) (mmap mcar frame))
(define (frame-values frame) (mmap mcdr frame))
(define (add-binding-to-frame! var val frame)
  (set! frame (mcons (mcons var val) frame)))

;;; NOT CHANGED -- LINK WITH THE NEW DEFS
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame (list->mlist vars) (list->mlist vals))
            base-env)
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

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))
(define the-global-environment (setup-environment))

(module+ test
  (define (test-eval e) (eval e the-global-environment))
  (test-eval '(+ 1 2 3))
  (test-eval '(if (< 3 5) 1 2))
  (test-eval '(if (< 5 3) 1 2))
  (test-eval '(if (< 5 3) 'false (cons 'a (cons (+ 3 5) '()))) )
  (test-eval '(if (< 3 5) 'true 'false) )
  'done)


