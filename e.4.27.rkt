#lang racket

;;; LAZY EVALUATOR FROM SECTION 4.2 OF
;;; STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS

(require "sicp.rkt")
(GETMOD 4 1 without eval apply)
(require scheme/mpair)
(require compatibility/mlist)

;;;SECTION 4.2.2

;;; Modifying the evaluator
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

(define input-prompt ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")
(define (driver-loop)
  "not tested"
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output
           (actual-value input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

;;; Representing thunks

;; non-memoizing version of force-it
(define (force-it-no-memo obj)
  (if (thunk? obj)
      (actual-value (thunk-exp obj) (thunk-env obj))
      obj))

(define (mtagged-list? exp tag)
  (if (mpair? exp)
      (eq? (mcar exp) tag)
      false))

;; thunks
(define (delay-it exp env) (mlist 'thunk exp env))
(define (thunk? obj) (mtagged-list? obj 'thunk))
(define (thunk-exp thunk) (mcar (mcdr thunk)))
(define (thunk-env thunk) (mcar (mcdr (mcdr thunk))))

;; "thunk" that has been forced and is storing its (memoized) value
(define (evaluated-thunk? obj) (mtagged-list? obj 'evaluated-thunk))
(define (thunk-value evaluated-thunk) (mcar (mcdr evaluated-thunk)))

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

'LAZY-EVALUATOR-LOADED

(module+ test
  (define (test-lazy-eval e) (actual-value e the-global-environment))
  
  (test-lazy-eval 1)
  (test-lazy-eval (+ 1 2 3))
  (test-lazy-eval '(define (try a b)
                     (if (= a 0)
                         1
                         b)))
  (test-lazy-eval '(try 0 (/ 1 0)))

  (test-lazy-eval '(define (abs x)
                     ((if (< 0 x)
                          +
                          -)
                      x)))
  (test-lazy-eval '(abs -10))

  "MUTABLE COUNT TEST"
  (test-lazy-eval '(define count 0))
  (test-lazy-eval '(define (id x)
                     (set! count (+ count 1))
                     x))
  (test-lazy-eval '(define w (id (id 10))))

  (test-lazy-eval 'count)

  (test-lazy-eval 'w)
  
  (test-lazy-eval 'count)
  
  'done)

(module+ export
  (provide actual-value
           the-global-environment
           mtagged-list?
           delay-it
           thunk?
           thunk-exp
           thunk-env
           evaluated-thunk?
           thunk-value
           input-prompt
           output-prompt
           ))
