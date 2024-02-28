#lang racket

(require "sicp.rkt")
(GETMOD 4 1)

;; analyze from 4.1.6, with clause from 4.3.3 added
;; and also support for Let
(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((permanent-assignment? exp) (analyze-permanent-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp)))
        ((amb? exp) (analyze-amb exp))
        ((ramb? exp) (analyze-ramb exp))
        ((if-fail? exp) (analyze-if-fail exp))
        ((require? exp) (analyze-require exp))
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

;;;Code from SECTION 4.3.3, modified as needed to run it

(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))

(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

;;; Random selection of ambigous branches
(define (ramb? exp) (tagged-list? exp 'ramb))
(define (ramb-choices exp) (cdr exp))

;;;Simple expressions

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))

(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env)
             fail)))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env)
               fail))))

;;; Conditional to catch a failure

(define (if-fail? exp) (tagged-list? exp 'if-fail))
(define (succ-expr exp) (cadr exp))
(define (fail-expr exp) (caddr exp))

(define (analyze-if-fail exp)
  (let ((succ-e (analyze (succ-expr exp)))
        (fail-e (analyze (fail-expr exp))))
    (lambda (env succeed fail)
      (succ-e env
              (lambda (value fail2)
                (succeed value fail2))
              (lambda ()
                (fail-e env succeed fail))))))

;;; require as a special form.

(define (require? exp) (tagged-list? exp 'require/sp))
(define (require-predicate exp) (cadr exp))

(define (analyze-require exp)
  (let ((pproc (analyze (require-predicate exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value fail2)
               (if (false? pred-value)
                   (fail2)
                   (succeed 'ok fail2)))
             fail))))

;;;Conditionals and sequences

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             ;; success continuation for evaluating the predicate
             ;; to obtain pred-value
             (lambda (pred-value fail2)
               (if (true? pred-value)
                   (cproc env succeed fail2)
                   (aproc env succeed fail2)))
             ;; failure continuation for evaluating the predicate
             fail))))

(define (analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
         ;; success continuation for calling a
         (lambda (a-value fail2)
           (b env succeed fail2))
         ;; failure continuation for calling a
         fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (and (null? procs)
         (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

;;; Permanent assignment

(define (permanent-assignment? exp)
  (tagged-list? exp 'p-set!))

(define (analyze-permanent-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (set-variable-value! var val env)
               (succeed 'ok fail2))
             fail))))

;;;Definitions and assignments

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (define-variable! var val env)
               (succeed 'ok fail2))
             fail))))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)        ; *1*
               (let ((old-value
                      (lookup-variable-value var env)))
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lambda ()    ; *2*
                            (set-variable-value! var
                                                 old-value
                                                 env)
                            (fail2)))))
             fail))))

;;;Procedure applications

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
             (lambda (proc fail2)
               (get-args aprocs
                         env
                         (lambda (args fail3)
                           (execute-application
                            proc args succeed fail3))
                         fail2))
             fail))))

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs) env
       ;; success continuation for this aproc
       (lambda (arg fail2)
         (get-args (cdr aprocs)
                   env
                   ;; success continuation for recursive
                   ;; call to get-args
                   (lambda (args fail3)
                     (succeed (cons arg args)
                              fail3))
                   fail2))
       fail)))

(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args)
                  fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))
          succeed
          fail))
        (else
         (error
          "Unknown procedure type -- EXECUTE-APPLICATION"
          proc))))

;;;amb expressions

(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices) env
             succeed
             (lambda ()
               (try-next (cdr choices))))))
      (try-next cprocs))))

(define (analyze-ramb exp)
  (lambda (env succeed fail)
    (ambeval
     (car (ramb-choices exp))
     env
     (lambda (v f)
       (define (try-next choices)
         (if (null? choices)
             (fail)
             (succeed
              (car choices)
              (lambda ()
                (try-next (cdr choices))))))
       (try-next (shuffle v)))
     fail)))

;;;Driver loop

(define input-prompt  ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval value:")

(define (driver-loop read)
  (define (internal-loop try-again)
    (let ((input (read)))
      (or (equal? input '(amb))
          (begin (prompt-for-input input-prompt)
                 (d ">" input)))
      (case input
        ('try-again (try-again))
        (else
         (or (equal? input '(amb))
             (begin (newline)
                    (display ";;; Starting a new problem ")))
         (ambeval input
                  the-global-environment
                  ;; ambeval success
                  (lambda (val next-alternative)
                    (or (equal? input '(amb))
                        (begin (newline)
                               (announce-output output-prompt)
                               (d (user-print val))))
                    (internal-loop next-alternative))
                  ;; ambeval failure
                  (lambda ()
                    (or (equal? input '(amb))
                        (begin (newline)
                               (announce-output ";;; There are no more values of")
                               (d (user-print input))))
                    (or (equal? input '(amb))
                        (driver-loop read))))))))
  (internal-loop
   (lambda ()
     (newline)
     (display ";;; There is no current problem")
     (driver-loop read))))

;;; Support for Let (as noted in footnote 56, p.428)

(define (let? exp) (tagged-list? exp 'let))
(define (let-bindings exp) (cadr exp))
(define (let-body exp) (cddr exp))

(define (let-var binding) (car binding))
(define (let-val binding) (cadr binding))

(define (make-combination operator operands)
  (cons operator operands))

(define (let->combination exp)
  ;; make-combination defined in earlier exercise
  (let ((bindings (let-bindings exp)))
    (make-combination (make-lambda (map let-var bindings)
                                   (let-body exp))
                      (map let-val bindings))))

'AMB-EVALUATOR-LOADED

(define (amb-test code)
  (define (read)
    (if (null? code)
        '(amb)
        (let ((v (car code)))
          (set! code (cdr code))
          v)))
  (driver-loop read))

(amb-test
 '((define (require p)
     (if (not p) (amb)))
   (define (>= a b)
     (< b a))))

(module+ test
  (amb-test '((define (prime-sum-pair list1 list2)
                (let ((a (an-element-of list1))
                      (b (an-element-of list2)))
                  (require (prime? (+ a b)))
                  (list a b)))

              (define (an-element-of items)
                (require (not (null? items)))
                (amb (car items) (an-element-of (cdr items))))

              (define (an-integer-starting-from n)
                (amb n (an-integer-starting-from (+ n 1))))))

  (amb-test '((+ 1 2 3)))

  (amb-test '((prime-sum-pair '(1 3 5 8) '(20 35 110))
              try-again
              try-again
              try-again
              (prime-sum-pair '(19 27 30) '(11 36 58))
              ))
  
  (amb-test
   '((define (an-integer-between a b)
       (if (<= a b)
           (amb a (an-integer-between (+ 1 a) b))
           (amb)))
     (define (a-pythagorean-triple-between low high)
       (let ((i (an-integer-between low high)))
         (let ((j (an-integer-between i high)))
           (let ((k (an-integer-between j high)))
             (require (= (+ (* i i) (* j j)) (* k k)))
             (list i j k)))))))
  
  (amb-test '((a-pythagorean-triple-between 10 20)))
  (amb-test '((a-pythagorean-triple-between 100 200)
              try-again
              try-again
              try-again
              try-again
              try-again
              try-again
              try-again
              try-again
              try-again
              try-again
              try-again
              try-again
              try-again
              try-again))
  (amb-test '((a-pythagorean-triple-between 1000 2000)))
  
  'done)

(module+ export
  (provide amb-test
           ))
