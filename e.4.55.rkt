#lang racket

(require "sicp.rkt")
(GETMOD 4 1)
(require "e.4.55.S.rkt")
(require "e.4.55.DB.rkt")

;;; QUERY SYSTEM FROM SECTION 4.4.4 OF
;;; STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS

;;; The Evaluator

(define (query/eval CALLER query frame-stream)
  (DEBUG/EVAL CALLER query frame-stream)
  (case (type query)
    ('and          (conjoin      (contents query) frame-stream))
    ('or           (disjoin      (contents query) frame-stream))
    ('not          (negate       (contents query) frame-stream))
    ('lisp-value   (lisp-value   (contents query) frame-stream))
    ('always-true  (always-true  (contents query) frame-stream))
    ('accumulation (accumulation (contents query) frame-stream))
    ('unique       (unique       (contents query) frame-stream))
    (else          (simple-query           query  frame-stream))))

;;; Simple queries

(define (simple-query query-pattern frame-stream)
  (S:flatmap
   (lambda (frame)
     (S:append-delayed
      (find-assertions query-pattern frame)
      (S:delay (apply-rules query-pattern frame))))
   frame-stream))

;;; Compound queries

(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (conjoin (rest-conjuncts conjuncts)
               (query/eval 'AND
                           (first-conjunct conjuncts)
                           frame-stream))))

(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      S:empty
      (S:interleave-delayed
       (query/eval 'OR
                   (first-disjunct disjuncts)
                   frame-stream)
       (S:delay (disjoin (rest-disjuncts disjuncts)
                       frame-stream)))))

;;; Filters

(define (negate operands frame-stream)
  (S:flatmap
   (lambda (frame)
     (if (S:null? (query/eval 'NOT
                              (negated-query operands)
                              (S:singleton frame)))
         (S:singleton frame)
         S:empty))
   frame-stream))

(define (execute exp)
  "use the-global-environment, not the-empty-environment!  operators
as + = - symbol-name ... are not bound in the empty environment."
  ;; (d "EXE\n" (pretty-format exp) "\n")
  (apply (eval (predicate exp) the-global-environment)
         (map (lambda (x) (eval x the-global-environment)) (args exp))))

(define (lisp-value call frame-stream)
  (S:flatmap
   (lambda (frame)
     (if (execute (instantiate call frame
                    (lambda (v)
                      (error "Unknown pat var -- LISP-VALUE" v))))
         (S:singleton frame)
         S:empty))
   frame-stream))

(define (always-true ignore frame-stream) frame-stream)

;;; Accumulation

(define (accumulation operands frame-stream)
  (define ACC (accum-init operands))
  (define op  (accum-op operands))
  (define var (accum-var operands))
  (S:for-each (lambda (frame)
                (let ((VAL (instantiate var frame
                             (lambda (v) (error "unknown var" v)))))
                  (if VAL
                      (set! ACC (execute (list op ACC VAL)))
                      (error "unknown varaiable" (accum-var operands)))))
              (query/eval "ACCUM"
                          (accum-query operands)
                          frame-stream))
  (S:singleton (unify-match ACC var '())))

;;; Unique

(define (unique operands frame-stream)
  (S:flatmap
   (lambda (frame)
     (let ((out (query/eval 'UNIQUE
                            (unique-query operands)
                            (S:singleton frame))))
       (if (S:null? (S:cdr out))
           out
           S:empty)))
   frame-stream))

;;; Finding Assertions by Pattern Matching

(define (find-assertions pattern frame)
  ;; (d "FIND-ASSERTIONS")
  (S:flatmap (lambda (datum)
               (check-an-assertion datum pattern frame))
             ((DB 'fetch-assertions) pattern frame)))
(define (check-an-assertion assertion query-pat query-frame)
  (let ((match-result
         (pattern-match query-pat assertion query-frame)))
    (if (eq? match-result 'failed)
        S:empty
        (S:singleton match-result))))
(define (pattern-match pat dat frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? pat dat) frame)
        ((var? pat) (extend-if-consistent pat dat frame))
        ((and (pair? pat) (pair? dat))
         (pattern-match (cdr pat)
                        (cdr dat)
                        (pattern-match (car pat)
                                       (car dat)
                                       frame)))
        (else 'failed)))
(define (extend-if-consistent var dat frame)
  (let ((binding (binding-in-frame var frame)))
    (if binding
        (pattern-match (binding-value binding) dat frame)
        (extend var dat frame))))

;;; Memoization

(define history-rules
  '())
(define dup/rule?
  (lambda (r)
   (member r history-rules)))
(define add/history
  (lambda (r)
    (and use/memoization
         (set! history-rules (cons r history-rules)))))
(define use/memoization
  true)

;;; Rules and Unification

(define (apply-rules pattern frame)
  (S:flatmap (lambda (rule)
               (apply-a-rule rule pattern frame))
             ((DB 'fetch-rules) pattern frame)))
(define (apply-a-rule rule query-pattern query-frame)
  (let ((clean-rule (rename-variables-in rule)))
    (let ((unify-result
           (unify-match query-pattern
                        (conclusion clean-rule)
                        query-frame)))
      
      (DEBUG/APPLY/RULE clean-rule
                        query-pattern
                        query-frame
                        (eq? 'failed unify-result))
      
      (define rule/memoized
        (instantiate query-pattern query-frame (lambda _ '__)))
      
      (cond ((eq? unify-result 'failed)
             S:empty)
            ((and use/memoization
                  (dup/rule? rule/memoized))
             S:empty)
            (else
             (add/history rule/memoized)
             (query/eval 'APPLY
                         (rule-body clean-rule)
                         (S:singleton unify-result)))))))
(define (rename-variables-in rule)
  (let ((rule-application-id (new-rule-application-id)))
    (define (tree-walk exp)
      (cond ((var? exp)
             (make-new-variable exp rule-application-id))
            ((pair? exp)
             (cons (tree-walk (car exp))
                   (tree-walk (cdr exp))))
            (else exp)))
    (tree-walk rule)))
(define (unify-match p1 p2 frame)
  ;; (d "--" p1 p2 frame)
  (define (iter p1 p2 frame)
    (cond ((eq? frame 'failed) 'failed)
          ((equal? p1 p2) frame)
          ((var? p1) (extend-if-possible p1 p2 frame))
          ((var? p2) (extend-if-possible p2 p1 frame)) ; {\em ; ***}
          ((and (pair? p1) (pair? p2))
           (iter (cdr p1)
                 (cdr p2)
                 (iter (car p1)
                       (car p2)
                       frame)))
          (else 'failed)))
  (iter p1 p2 frame))
(define (extend-if-possible var val frame)
  (let ((binding (binding-in-frame var frame)))
    (cond (binding
           (unify-match
            (binding-value binding) val frame))
          ((var? val)                     ; {\em ; ***}
           (let ((binding (binding-in-frame val frame)))
             (if binding
                 (unify-match
                  var (binding-value binding) frame)
                 (extend var val frame))))
          ((depends-on? val var frame)    ; {\em ; ***}
           'failed)
          (else (extend var val frame)))))
(define (depends-on? exp var frame)
  (define (tree-walk e)
    (cond ((var? e)
           (if (equal? var e)
               true
               (let ((b (binding-in-frame e frame)))
                 (if b
                     (tree-walk (binding-value b))
                     false))))
          ((pair? e)
           (or (tree-walk (car e))
               (tree-walk (cdr e))))
          (else false)))
  (tree-walk exp))

(define rule-counter 0)
(define (new-rule-application-id)
  (set! rule-counter (+ 1 rule-counter))
  rule-counter)
(define (make-new-variable var rule-application-id)
  (cons '? (cons rule-application-id (cdr var))))

;;; Query syntax procedures

(define (type exp)
  (if (pair? exp)
      (car exp)
      (error "Unknown expression TYPE" exp)))
(define (contents exp)
  (if (pair? exp)
      (cdr exp)
      (error "Unknown expression CONTENTS" exp)))
(define (assertion-to-be-added? exp)
  (eq? (type exp) 'assert!))
(define (add-assertion-body exp)
  (car (contents exp)))

(define (empty-conjunction? exps) (null? exps))
(define (first-conjunct exps) (car exps))
(define (rest-conjuncts exps) (cdr exps))

(define (empty-disjunction? exps) (null? exps))
(define (first-disjunct exps) (car exps))
(define (rest-disjuncts exps) (cdr exps))

(define (negated-query exps) (car exps))

(define (predicate exps) (car exps))
(define (args exps) (cdr exps))

(define (accum-op exps) (car exps))
(define (accum-init exps) (cadr exps))
(define (accum-var exps) (caddr exps))
(define (accum-query exps) (cadddr exps))

(define (unique-query exps) (car exps))

;;; Frames and bindings

(define (make-binding variable value)
  (cons variable value))
(define (binding-variable binding)
  (car binding))
(define (binding-value binding)
  (cdr binding))
(define (binding-in-frame variable frame)
  (assoc variable frame))
(define (extend variable value frame)
  (cons (make-binding variable value) frame))

;;; Debugging

(define APPLY/RULE/DBG #f)
(define EVAL/DBG       #f)
(define FRAME/DBG      #f)

(define dbg/head
  (lambda (ch title)
    (~a "--" (make-string 40 ch) "--" title "--" rule-counter "--")))

(define DEBUG/APPLY/RULE
  (lambda (R PAT FRAME UNIF/FAILED?)
    (define xx
      (lambda (M xx)
        (d (~a M
               " "
               (pretty-format
                (instantiate xx '() contract-question-mark))))))
    (and
     APPLY/RULE/DBG
     (if (not UNIF/FAILED?)
         (begin
           (d)
           (d (~a (dbg/head #\. "APPLY/RULE:")
                  (if UNIF/FAILED? "FAIL" "SUCCEED")))
           (xx "---CONCLUSION---" (conclusion R))
           (xx "---PATTERN---"    PAT)
           (xx "---BODY---"       (rule-body R))
           (display/frame FRAME))
         (and UNIF/FAILED?
              (xx "XX" R))))
    R))

(define DEBUG/EVAL
  (lambda (CALLER query frame-stream)
    (define STR
      (lambda ()
        (~a (dbg/head #\# "--QUERY/EVAL:")
            "[" CALLER "]"
            "-" (type query) "-")))
    
    (and
     EVAL/DBG
     (begin
       (d)
       (d (STR))
       (pretty-print (instantiate (contents query) '() contract-question-mark))
       (and FRAME/DBG
            (begin (d "STREAM OF FRAMES: ")
                   (S:for-each display/frame frame-stream)))))))

(define (display/frame frame)
  (define (f/var variable)
    (if (number? (cadr variable))
        (~a (caddr variable) "/" (cadr variable))
        (cadr variable)))  
  (define (f/val value)
    (cond ((var? value) (f/var value))
          ((and (pair? value)
                (var? (car value)))
           (cons (f/var (car value))
                 (f/val (cdr value))))
          (else value)))
  (define (STR var val)
    (~a (~a #:width 30 #:pad-string "." (f/var var))
        "=>"
        (f/val val)))
  
  (and
   FRAME/DBG
   (begin
     (newline)
     (d "FRAME {")
     (for-each (lambda (frame)
                 (let ((var (binding-variable frame))
                       (val (binding-value frame)))
                   (or (var? var)
                       (error "wrong frame" f))
                   (d "    " (STR var val))))
               frame)
     (d "}")))
  frame)

;;; The Driver Loop and Instantiation

(define input-prompt ";;; Query input:\n")
(define output-prompt ";;; Query results:\n")
(define (query-driver-loop k)
  (if (null? k)
      'done
      (let ((q (query-syntax-process (car k))))
        (or (eq? (car q) 'COMM)
            (prompt-for-input
             (string-upcase
              (~a input-prompt (pretty-format q)))))
        (cond ((assertion-to-be-added? q)
               ((DB 'add-rule-or-assertion!) (add-assertion-body q))
               (newline)
               (display "Assertion added to data base.")
               (newline)
               (query-driver-loop (cdr k)))
              ((eq? (car q) 'COMM)
               (newline)
               (apply/scheme/internal d (cdr q))
               (o (make-string 40 #\=))
               (newline)
               (query-driver-loop (cdr k)))
              (else
               (newline)
               (display output-prompt)
               (S:display
                (S:map
                 (lambda (frame)
                   (instantiate q frame (lambda (v) (contract-question-mark v))))
                 (query/eval 'REPL q (S:singleton '()))))
               (query-driver-loop (cdr k)))))))
(define (instantiate exp frame unbound-var-handler)
  (define (copy exp)
    (cond ((var? exp)
           (let ((binding (binding-in-frame exp frame)))
             (if binding
                 (copy (binding-value binding))
                 (unbound-var-handler exp))))
          ((pair? exp)
           (cons (copy (car exp)) (copy (cdr exp))))
          (else exp)))
  (copy exp))

(define test/
  (lambda (q*)
    (set! use/memoization false)
    (query-driver-loop q*)))
(define test/memo
  (lambda (q*)
    (set! use/memoization true)
    (query-driver-loop q*)))

(module+ test

  (initialize-data-base microshaft-data-base)

  (test/ '((supervisor ?x (Bitdiddle Ben))
           (job ?name (accounting . ?job))
           (address ?name (Slumerville . ?address))))
  
  (test/ '((same ?x ?x)))
  
  (test/ '((lives-near ?x ?y)
           (COMM "ORDER 1")
           (and (supervisor ?x ?y)
                (not (job ?x (computer programmer))))
           (COMM "REVERSED ORDER")
           (and (not (job ?x (computer programmer)))
                (supervisor ?x ?y))
           (and (and (supervisor ?x ?y)
                     (not (job ?x (computer programmer))))
                (and (not (job ?x (computer programmer)))
                     (supervisor ?x ?y)))
           (and (and (not (job ?x (computer programmer)))
                     (supervisor ?x ?y))
                (and (supervisor ?x ?y)
                     (not (job ?x (computer programmer)))))))
  
  ((DB 'initialize-data-base) a-data-base)
  (test/ '((append (a b) (c d) ?r)
           (append (a b) ?w (a b c d))))

  'done)

(module+ export
  (provide
   rule-counter
   test/
   test/memo
   initialize-data-base
   microshaft-data-base
   instantiate
   contract-question-mark
   query/eval
   S:display
   ))


