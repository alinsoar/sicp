#lang racket

(include "e.4.55.DB.XX.rkt")

;;; the stream library 

(define S:empty '())
(define S:empty? (lambda (stream) (null? stream)))
(define S:head (lambda (stream) (car stream)))
(define S:tail (lambda (stream) ((cdr stream))))
(define (S:singleton x)
  (cons x (lambda () S:empty)))
(define (S:map proc stream)
  (if (S:empty? stream)
      S:empty
      (cons (proc (S:head stream))
            (lambda ()
              (S:map proc (S:tail stream))))))
(define (S:interleave s1 delayed-s2)
  (if (S:empty? s1)
      (delayed-s2)
      (cons (S:head s1)
            (lambda ()
              (S:interleave (delayed-s2)
                            (lambda () (S:tail s1)))))))
(define (S:flatmap proc stream)
  (define (iter stream)
    (if (S:empty? stream)
        S:empty
        (S:interleave
         (S:head stream)
         (lambda ()
           (iter (S:tail stream))))))
  (iter 
   (S:map proc stream)))
(define (S:display s)
  (if (S:empty? s)
      (newline)
      (begin (displayln
              (string-upcase
               (pretty-format (S:head s)
                              (pretty-print-columns))))
             (S:display (S:tail s)))))

;;; The Driver Loop and Instantiation

(define (query-syntax-process exp)
  (map-over-symbols expand-question-mark exp))
(define (map-over-symbols proc exp)
  (cond ((pair? exp)
         (cons (map-over-symbols proc (car exp))
               (map-over-symbols proc (cdr exp))))
        ((symbol? exp) (proc exp))
        (else exp)))
(define (expand-question-mark symbol)
  (let ((chars (symbol->string symbol)))
    (if (string=? (substring chars 0 1) "?")
        (list '?
              (string->symbol
               (substring chars 1 (string-length chars))))
        symbol)))
(define (contract-question-mark var)
  (string->symbol
   (string-append
    "?"
    (if (fresh-var? var)
        (string-append (symbol->string (caddr var))
                       "-"
                       (number->string (cadr var)))
        (symbol->string (cadr var))))))

(define input-prompt "\n\n--- Query input: ")
(define output-prompt "\n--- Query results:\n")
(define (query-driver-loop k) 
  (if (null? k)
      'done
      (let ((q (query-syntax-process (car k)))) 
        (display input-prompt)
        (display (car k)) 
        (display output-prompt)
        (S:display
         (S:map
          (lambda (frame)
            (instantiate q frame
              (lambda (v) (contract-question-mark v))))
          (query/eval q (S:singleton S:empty))))
        (query-driver-loop (cdr k)))))

(define (instantiate exp frame unbound-var-handler)
  (define (copy exp)
    (cond ((var? exp)
           (let ((binding (binding-in-substitution exp frame)))
             (if binding
                 (copy (binding-value binding))
                 ;; called for non-ground terms
                 (unbound-var-handler exp))))
          ((pair? exp)
           (cons (copy (car exp)) (copy (cdr exp))))
          (else exp)))
  (copy exp))

;;; the Data Base

(define THE-RULES S:empty)
(define (initialize-data-base rules-and-assertions)
  (define (deal-out r-and-a)
    (let ((old-rules THE-RULES)
          (rule (car r-and-a)))
      (set! THE-RULES 
            (cons 
             (query-syntax-process (cdr rule))
             (lambda () old-rules)))
      (or (null? (cdr r-and-a))
          (deal-out (cdr r-and-a)))))
  (deal-out rules-and-assertions)
  'init-database-ok)

;;; QUERY SYSTEM -- The Evaluator

(define (query/eval query frame-stream)
  (case (type query)
    ('and (conjoin (contents query) frame-stream))
    ('or (disjoin (contents query) frame-stream))
    ('not (negate (contents query) frame-stream))
    ('= (unification (contents query) frame-stream))
    ('check-predicate (machine-value (contents query) frame-stream))
    (else (apply-query query frame-stream))))

;;; Simple queries

(define (apply-query query-pattern frame-stream)
  (S:flatmap
   (lambda (frame)
     (S:flatmap (lambda (rule)
                  (apply-a-rule rule query-pattern frame))
                THE-RULES))
   frame-stream))

;;; Compound queries

(define (conjoin conjuncts frame-stream)
  (if (no-rand? conjuncts)
      frame-stream
      (conjoin (rest-operands conjuncts)
               (query/eval (first-operand conjuncts)
                           frame-stream))))

(define (disjoin disjuncts frame-stream)
  (if (no-rand? disjuncts)
      S:empty
      (S:interleave
       (query/eval (first-operand disjuncts)
                   frame-stream)
       (lambda () (disjoin (rest-operands disjuncts)
                      frame-stream)))))

;;; Filters

(define (negate operands frame-stream)
  (S:flatmap
   (lambda (frame)
     (let ((singleton (S:singleton frame)))
       (if (S:empty? (query/eval (first-operand operands)
                                 singleton))
           singleton
           S:empty)))
   frame-stream))

(define (unification operands frame-stream)
  (let ((first (car operands))
        (second (cadr operands)))
    (S:flatmap (lambda (frame)
                 (unify-match first second frame))
               frame-stream)))

(define machine-value
  (let ((machine-functions 
         `((test123 . ,(lambda (r)
                         (define aa
                           (lambda(x)
                             (if (eq? 'zero x) 0 (+ 1 (aa (cadr x))))))
                         (< (aa r) 2))))))
    (lambda (operands frame-stream)
      (let ((args (car operands))
            (function (cadr operands)))
        (S:flatmap
         (lambda (substitution)
           (let ((w (apply (cdr (assoc function machine-functions))
                           (map (lambda (a) 
                                  (instantiate a 
                                      substitution
                                    (lambda (v) (contract-question-mark v))))
                                args))))
             (if w
                 (S:singleton substitution)
                 S:empty)))
         frame-stream)))))

;;; Rules and Unification

(define (apply-a-rule rule query-pattern query-frame)
  (define iter
    (lambda (clean-rule frame)
      (if (null? clean-rule)
          frame
          (let ((f/stream (query/eval (car clean-rule) frame)))
            (iter (cdr clean-rule) f/stream)))))
  (let* ((clean-rule (make-fresh-vars rule))
         (unify-result
          (unify-match query-pattern (car clean-rule) query-frame)))
    (if (S:empty? unify-result)
        S:empty 
        (iter (cdr clean-rule) unify-result))))

;;; invariant: if the result is a variable `x`, then `x` is fresh,
;;; otherwise `v` is bound
(define walk
  (lambda (v s)
    (let ((a (and (var? v) (binding-in-substitution v s))))
      (if (pair? a)
          (walk (cdr a) s)
          v))))

(define make-fresh-vars
  (let ((rule-counter 0)) 
    (define (tree-walk exp)
      (cond ((var? exp) (make-fresh-var rule-counter exp))
            ((pair? exp) 
             (cons (tree-walk (car exp))
                   (tree-walk (cdr exp))))
            (else exp)))
    (lambda (rule)
      (set! rule-counter (+ 1 rule-counter))
      (tree-walk rule))))

(define (unify-match p1 p2 s)
  (define (iter p1 p2 s)
    (if (eq? s 'failed) 'failed
        (let ((p1 (walk p1 s))
              (p2 (walk p2 s)))
          (cond ((equal? p1 p2) s)
                ((var? p1) (extend-if-possible p1 p2 s))
                ((var? p2) (extend-if-possible p2 p1 s))
                ((and (pair? p1) (pair? p2))
                 (iter (cdr p1) (cdr p2)
                       (iter (car p1) (car p2)
                             s)))
                (else 'failed)))))
  (let ((unifier (iter p1 p2 s)))
    (if (eq? unifier 'failed)
        S:empty
        (S:singleton unifier))))
(define (extend-if-possible var val s)
  "add the association (VAR . VALUE) in substitution S, if possible"
  (cond
   ((self-referenced? val var s)
    'failed)
   (else (extend-substitution var val s))))
(define (self-referenced? x v s)
  "true when the variable V depends on X"
  (if (var? v) (eqv? v x)
      (and (pair? v)
           (or (self-referenced? x (car v) s)
               (self-referenced? x (cdr v) s)))))

;;; substitutions and bindings

(define (make-binding var value) (cons var value))
(define (binding-var binding) (car binding))
(define (binding-value binding) (cdr binding))
(define (binding-in-substitution var s) (assoc var s))
(define (extend-substitution var value s) 
  (cons (make-binding var value) s))

(define (fresh-var? exp) (number? (cadr exp)))
(define (var? exp) (and (pair? exp) (eq? (car exp) '?)))
(define (make-fresh-var counter var) (list '? counter (cadr var)))

;;; Query rule syntax procedures
(define (type exp)
  (if (pair? exp)
      (car exp)
      (error "Unknown expression TYPE" exp)))
(define (contents exp)
  (if (pair? exp)
      (cdr exp)
      (error "Unknown expression CONTENTS" exp)))

(define no-rand? (lambda (e) (null? e)))
(define (first-operand exps) (car exps))
(define (rest-operands exps) (cdr exps))

(test)
