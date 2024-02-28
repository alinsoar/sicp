#lang racket

(require "sicp.rkt")
(GETMOD 4 1 without
        eval apply
        enclosing-environment
        first-frame
        frame-variables
        frame-values
        the-empty-environment
        add-binding-to-frame!
        )
(GETMOD 4 27 without actual-value)
(require scheme/mpair)
(require compatibility/mlist)

;;; Modifying the evaluator
(define (eval exp env)
  '(f "~nEXPRESSION: ~a" exp)
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
         (apply (actual-value (operator exp) env)
                (operands exp)
                env))
        (else
         (error "Unknown expression type -- EVAL" exp))))
(define (actual-value exp env)
  (force-it (eval exp env)))

(define (apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env)))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters-names procedure)
           (list-of-args arguments
                         (procedure-parameters-types procedure)
                         env)
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((leftmost-value (eval (first-operand exps) env)))
        (cons leftmost-value (list-of-values (rest-operands exps) env)))))
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)
(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps)
                                env))))
(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

;;; CHANGED DEFINITIONS
(define (eval-definition exp env)
  (define (make-params params)
    (define (iter p)
      (cond ((null? p) '())
            ((symbol? (car p))
             (cons (cons (car p) 'call)
                   (iter (cdr p))))
            (else (cons (cons (caar p) (cadar p))
                        (iter (cdr p))))))
    (iter params))
  
  (let*  ((value (definition-value exp)))
    (define-variable!
      (definition-variable exp)
      (eval (if (lambda? value)
                (make-lambda (make-params (lambda-parameters value))
                             (lambda-body value))
                value) env)
      env))
  'ok)
(define (list-of-args exps types env)
  '(d "###" exps types)
  (if (no-operands? exps)
      '()
      (let ((a (car exps))
            (t (car types)))
        (cons (cond ((argument-type-call?     t) (force-it-no-memo (eval a env)))
                    ((argument-type-lazymemo? t) (delay-it a env))
                    ((argument-type-lazy?     t) (delay-it-no-memo a env))
                    (else (error "ERROR -- LIST-OF-ARGS" exps types)))
              
              (list-of-args (rest-operands exps)
                            (cdr types)
                            env)))))

(define (procedure-parameters-names procedure)
  (map car (procedure-parameters procedure)))
(define (procedure-parameters-types procedure)
  (map cdr (procedure-parameters procedure)))

(define (argument-type-call? p) (eq? p 'call))
(define (argument-type-lazy? p) (eq? p 'lazy))
(define (argument-type-lazymemo? p) (eq? p 'lazy-memo))

(define (delay-it-no-memo exp env) (mlist 'thunk-no-memo exp env))
(define (thunk-no-memo? obj) (mtagged-list? obj 'thunk-no-memo))

;;; Representing thunks

;; memoizing version of force-it
(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value
                        (thunk-exp obj)
                        (thunk-env obj))))
           (set-mcar!       obj  'evaluated-thunk)
           (set-mcar! (mcdr obj) result) ; replace exp with its value
           (set-mcdr! (mcdr obj) '())    ; forget unneeded env
           result))
        ((thunk-no-memo? obj)
         (actual-value (thunk-exp obj) (thunk-env obj)))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))
(define (force-it-no-memo obj)
  (if (thunk? obj)
      (actual-value (thunk-exp obj) (thunk-env obj))
      obj))

(module+ test
  (define (test-lazy-eval e) (actual-value e the-global-environment))
  
  (test-lazy-eval '(define (f a (b lazy) c (d lazy-memo) print-vars)
                     (cond ((= a 0) 'ok)
                           ((= a 1) b)
                           ((= a 2) (+ d 3))
                           (else 'ok))
                     (print-environment 'env print-vars)))
  
  (list (car (test-lazy-eval 'f))
        (cadr (test-lazy-eval 'f))
        (caddr (test-lazy-eval 'f))) 

  "LAZY PARAM B is evaluated and remains before and after the evaluation THUNK-NO-MEMO"
  (test-lazy-eval '(f (- 2 2) (/ 1 0) 3 (/ 1 0) '(b d)))

  "LAZY-MEMO PARAM D becomes, after evaluation with primitive call '+, EVALUATED-THUNK"
  (test-lazy-eval '(f 2 (+ 3 4) 3 (/ 2 1) '(d)))

  "CALL PARAMS A & C are evaluated at call."
  (test-lazy-eval '(f (- 2 2) (/ 1 0) (/ 3 3) (/ 1 0) '(a c)))
  
  'done)

