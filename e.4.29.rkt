#lang racket

(require "sicp.rkt")
(GETMOD 4 1 without eval apply)
(GETMOD 4 27 without
        actual-value)

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
  ((force-optional-memoization env) (eval exp env)))
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

(define (force-optional-memoization env)
  (let ((memo? (eval 'memo? the-global-environment)))
    (cond ((eq? memo? true)
           force-it)
          ((and (thunk? memo?) (thunk-exp memo?))
           force-it)
          ((eq? memo? false)
           force-it-no-memo)
          ((and (thunk? memo?) (not (thunk-exp memo?)))
           force-it-no-memo)
          ((and (evaluated-thunk? memo?) (thunk-exp memo?))
           force-it)
          ((and (evaluated-thunk? memo?) (not (thunk-exp memo?)))
           force-it-no-memo)
          (else (error "ERROR -- FORCE-OPTIONAL-MEMOIZATION" memo?)))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((leftmost-value (eval (first-operand exps) env)))
        (cons leftmost-value (list-of-values (rest-operands exps) env)))))
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

;;; Representing thunks

;; memoizing version of force-it
(define (force-it obj)
  '(d "MEMO")
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

(define (force-it-no-memo obj)
  '(d "NO MEMO")
  (if (thunk? obj)
      (actual-value (thunk-exp obj) (thunk-env obj))
      obj))

(module+ test
  (define (test-lazy-eval e) (actual-value e the-global-environment))
  (define-variable! 'memo? true the-global-environment)
  "MEMO?"
  (test-lazy-eval 'memo?)
  (test-lazy-eval '(set! memo? false))
  (test-lazy-eval 'memo?)

  "LOOKUP TESTS"
  (test-lazy-eval 'true)
  (test-lazy-eval 'false)
  (test-lazy-eval '+)
  (test-lazy-eval '(+ 1 2 3))

  (test-lazy-eval '(define fib
                     (lambda (n)
                       (define (fib n)
                         (if (< n 2)
                             n
                             (+ (fib (- n 1))
                                (fib (- n 2)))))
                       (if (<= 0 n)
                           (fib n)
                           (cons "ERROR: negative" (cons n '()))))))

  (test-lazy-eval
   '(define (wrong-count-time f arg use-memoization?)
      (set! memo? use-memoization?)
      (println "MEMOIZATION = " (if memo? "ON" "OFF") "; ")
      ((lambda (t0)
         (println "\t" 'F arg "=" (f arg))
         (println "WRONG RUNNING TIME =" (- (time) t0) ""))
       (time))
      (println)
      ))
  (test-lazy-eval
   '(define (count-time f arg use-memoization?)
      (set! memo? use-memoization?)
      (println "MEMOIZATION = " (if memo? "ON" "OFF") "; ")
      (define t0 (time))                ; DEFINE EVALUATES THE ARGUMENT AT APPLICATION
      (println "\t" 'F arg "=" (f arg))
      (define t1 (time))
      (println "RUNNING TIME =" (- t1 t0))
      (println)))
  "COUNT TIME RUNNING FIB WITH AND WITHOUT MEMOIZATION -- DEFINE DOES NOT DELAY ITS VALUE"
  (test-lazy-eval '(count-time fib 20 false))
  (test-lazy-eval '(count-time fib 20 true))
  "COUNT TIME RUNNING FIB ON AN INVALID INPUT"
  (test-lazy-eval '(count-time fib -20 true))
  "BAD COUNTING OF TIME -- LAMBDA PARAMETERS ARE DELAYED"
  (test-lazy-eval '(wrong-count-time fib 10 false))
  (test-lazy-eval '(wrong-count-time fib 10 true))
  'done)

