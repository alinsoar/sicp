#lang racket

(require "sicp.rkt")
(GETMOD 4 1 without eval apply)
(GETMOD 4 27 orig:)
(GETMOD 4 27 without actual-value)

;;; Modifying the evaluator
(define (eval exp env)
  '(f "~nEXPRESSION: ~a" exp)
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
        ((application? exp)             ; clause from book
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
          (list-of-arg-values arguments env))) ; changed
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-delayed-args arguments env) ; changed
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
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)
(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps)
                                env))))
(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps) env)
            (list-of-delayed-args (rest-operands exps)
                                  env))))
(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

;; '(define (eval-sequence exps env)
;;   (cond ((last-exp? exps) (eval (first-exp exps) env))
;;         (else (eval (first-exp exps) env)
;;               (eval-sequence (rest-exps exps) env))))
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (actual-value (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))


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
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))

(module+ test
  (define (test-lazy-eval e) (actual-value e the-global-environment))
  (define (test-orig-lazy-eval e) (orig:actual-value e the-global-environment))
  (test-lazy-eval '(+ 1 2 3))
  
  "** PART A -- BEN PRETENDS THE ORIGINAL NON-FORCING FOR SEQUENCES IS GOOD"
  (test-orig-lazy-eval
   '(define (for-each proc items)
      (if (null? items)
          'done
          (begin (proc (car items))
                 (for-each proc (cdr items))))))
  (test-orig-lazy-eval
   '(for-each (lambda (x) (newline) (display "==>" x))
              (list 57 321 88)))

  "** PART B -- BEN'S ORIGINAL"
  (test-orig-lazy-eval
   '(define (p1 x)
      (set! x (cons x '(2)))
      x))
  (test-orig-lazy-eval
   '(define (p2 x)
      (define (p e)
        e
        x)
      (p (set! x (cons x '(2))))))

  (test-orig-lazy-eval '(p1 1))
  (test-orig-lazy-eval '(p2 1))
  
  "--- ARGUMENTS OF LAMBDA ARE DELAYED"
  (test-lazy-eval '(p1 1))
  (test-lazy-eval '(p2 1))

  "PART C -- FOREACH WORKS WITH THE FORCING OF SEQUENCES THE SAME AS BEFORE"
  "NEWLINE AND DISPLAY ARE PRIMITIVES, SO EVAL CALLS THEM"
  (test-lazy-eval
   '(for-each (lambda (x) (newline) (display "==>" x))
              (list 57 321 88)))

  "PART D -- CY'S VERSION IS BETTER IN DEALING WITH MUTATION"
  (test-lazy-eval
   '(define (integers-from n)
      (print n ".")
      (cons n (lambda ()
                ((lambda (e) n e)
                 (set! n (+ n 1)))
                (integers-from n)))))
  
  (test-orig-lazy-eval '(car (integers-from 0)))
  (test-orig-lazy-eval '(car ((cdr (integers-from 0)))))
  (test-orig-lazy-eval '(car ((cdr ((cdr (integers-from 0)))))))
  (test-orig-lazy-eval '(car ((cdr ((cdr ((cdr (integers-from 0)))))))))

  (test-lazy-eval '(car (integers-from 0)))
  (test-lazy-eval '(car ((cdr (integers-from 0)))))
  (test-lazy-eval '(car ((cdr ((cdr (integers-from 0)))))))
  (test-lazy-eval '(car ((cdr ((cdr ((cdr (integers-from 0)))))))))
  
  
  'done)

