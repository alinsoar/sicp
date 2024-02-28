#lang racket

(require "sicp.rkt")
(GETMOD 4 1)
(GETMOD 4 55 q:)
(require "e.4.55.S.rkt")
(require "e.4.55.DB.rkt")

;;; The Evaluator

(define (query/eval CALLER query frame-stream)
  ;; (DBG/EVAL CALLER query frame-stream)
  (case (type query)
    ('and          (conjoin      (contents query) frame-stream))
    ('or           (disjoin      (contents query) frame-stream))
    ('not          (negate       (contents query) frame-stream))
    ('lisp-value   (lisp-value   (contents query) frame-stream))
    ('always-true  (always-true  (contents query) frame-stream))
    (else          (simple-query           query  frame-stream))))

;;; Simple queries

(define (simple-query query-pattern frame-stream)
  ;; (DBG/FRAMES frame-stream 'SHOW)
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

(define (always-true ignore frame-stream)
  frame-stream)

;;; Finding Assertions by Pattern Matching

(define (find-assertions pattern frame)
  (S:flatmap (lambda (datum)
               (check-an-assertion datum pattern frame))
             ((DB 'fetch-assertions) pattern frame)))
(define (check-an-assertion assertion query-pat query-frame)
  (let ((match-result (pattern-match query-pat assertion query-frame)))
    (if (match-result 'FAILED?)
        S:empty
        (S:singleton match-result))))
(define (pattern-match pat dat frame)
  (cond ((frame 'FAILED?) frame)
        ((equal? pat dat) frame)
        ((var? pat) (extend-if-consistent pat dat frame))
        ((and (pair? pat) (pair? dat))
         (pattern-match (cdr pat)
                        (cdr dat)
                        (pattern-match (car pat)
                                       (car dat)
                                       frame)))
        (else FRAME-FAIL)))
(define (extend-if-consistent var dat frame)
  (let ((binding ((frame 'LOOKUP) var)))
    (if binding
        (pattern-match (binding-value binding) dat frame)
        ((frame 'EXTEND) var dat 'M))))

;;; Bindings

(define (make-binding variable value)
  (list (cons variable value)))
(define (binding? x)
  (and (pair? x)
       (var? (car x))))
(define (binding-variable binding)
  (car binding))
(define (binding-value binding)
  (cdr binding))

;;; Frames

(define FRAME
  (let ((id/count (let ((N -1)) (lambda () (set! N (+ 1 N)) N))))
    (lambda (parent)
      (lambda (bindings S)

        (define ID (id/count))
        
        (define (binding-in-frame variable)
          (cond ((assoc variable bindings) => (lambda (b) b))
                (else ((parent 'LOOKUP) variable))))
        (define (extend variable value M)
          ((FRAME SELF)
           (make-binding variable value)
           M))

        (define (show . all?)
          (cons bindings (parent 'SHOW)))

        (define str
          (lambda ()
            (define arrow
              (lambda (a)
                (~a (~20 (if (fresh? (car a))
                             (~a '__FRESH: (caddar a))
                             (cadar a)))
                    "==> " (cdr a))))
            (define line
              (lambda (a) (~a "\t." (arrow a) "\n")))
            (~a "\n\t-" ID ":" S "----\n"
                (foldl (lambda (a r) (~a (line a) r)) "\t|"
                       bindings)
                (parent 'STR))))

        (define SELF
          (lambda (m)
            (cond ((eq? m 'LOOKUP)    binding-in-frame)
                  ((eq? m 'EXTEND)    extend)
                  ((eq? m 'FAILED?)   (eq? bindings 'FAIL))
                  ((eq? m 'SHOW)      (show))
                  ((eq? m 'STR)       (str))
                  ((eq? m 'ID)        ID)
                  (else (error "FRAME --" m)))))
        SELF))))

(define EMPTY-FRAME
  (letrec ((SELF
            (lambda (m)
              (cond ((eq? m 'LOOKUP)  (lambda _ false))
                    ((eq? m 'EXTEND)  (lambda (var val m)
                                        ((FRAME SELF)
                                         (make-binding var val)
                                         m)))
                    ((eq? m 'FAILED?) false)
                    ((eq? m 'SHOW)   '())
                    ((eq? m 'ID)     'EMPTY)
                    ((eq? m 'STR)    "\n\t*")
                    (else (error "EMPTY FRAME --" m))))))
    SELF))

(define FRAME-FAIL
  (lambda (m)
    (cond ((eq? m 'LOOKUP)  (error "failure"))
          ((eq? m 'EXTEND)  (error "cannot extend a failure"))
          ((eq? m 'FAILED?) true)
          ((eq? m 'SHOW)    'FAILED)
          ((eq? m 'ID)      'FAIL)
          ((eq? m 'STR)     "FAIL")
          (else (error "FAILED FRAME --" m)))))

;;; Rules and Unification

(define apply-rules
  (lambda (pattern frame)
    (let ((RULES ((DB 'fetch-rules) pattern frame)))
      ;; (DBG/RULES RULES 'KOUNTER pattern)
      (S:flatmap (lambda (rule)
                   (apply-a-rule rule pattern frame))
                 RULES))))

(define fresh
  (let ((k -1))
    (lambda ()
      (set! k (add1 k))
      (list '? (string->symbol "__FRESH") k))))

(define fresh?
  (lambda (v)
    (and (var? v)
         (= 3 (length v))
         (equal? '__FRESH (cadr v)))))

(define resolve-right-values
    (lambda (pattern env k)
      "rewrite PATTERN looking for values in ENV and collect new fresh
variables."
      (define (iter e b co)
        ;; (d "**" e ":" b)
        (cond ((and (var? e) (assoc e b)) =>
               (lambda (w)
                 (co (cdr w) b '())))
              ((and (var? e)
                    ((env 'LOOKUP) e)) =>
                    (lambda (x)
                      (let ((v (binding-value x)))
                        (if (var? v)
                            (iter v b co)
                            (co v b '())))))
              ((var? e)
               (let ((v (fresh)))
                 (co v (cons (cons e v) b)
                     (list v))))
              ((pair? e)
               (iter (car e)
                     b
                     (lambda (a b1 fs1)
                       (iter (cdr e)
                             b1
                             (lambda (d b2 fs2)
                               (co (cons a d) b2
                                   (append fs1 fs2)))))))
              (else
               (co e b '()))))
      (iter pattern '() k)))

(define (apply-a-rule rule query-pattern env)
  ;; STEP 1 -- rewrite pattern
  (resolve-right-values
   query-pattern
   env
   (lambda (right-vals rf fresh-list)
     ;; (DBG/APPLY rule query-pattern right-vals fresh-list env)
     ;; 
     ;; STEP 2 -- unify rewritten pattern with rule head in empty frame
     (let ((unif__ (unify-match right-vals (conclusion rule) EMPTY-FRAME)))
       (if (eq? unif__ FRAME-FAIL)
           S:empty
           ;; STEP 3 -- apply rule body in unified frame
           (let ((R (query/eval 'APPLY
                                (rule-body rule)
                                (S:singleton unif__))))
             ;; STEP 4 -- replace the variables from fresh-list with
             ;; constant values or variables from env.
             (S:map (lambda (fr)
                      ((FRAME ((FRAME env) rf 'RV))
                       (map (lambda (t)
                              (cons t
                                    (instantiate t fr
                                      (lambda (x) (d "!!FRESH" x) (fresh)))))
                            fresh-list)
                       'EX))
                    R)))))))

(define (unify-match p1 p2 frame)
  (define (iter p1 p2 frame)
    (cond ((frame 'FAILED?) frame)
          ((equal? p1 p2) frame)
          ((var? p1) (extend-if-possible p1 p2 frame))
          ((var? p2) (extend-if-possible p2 p1 frame))
          ((and (pair? p1) (pair? p2))
           (iter (cdr p1)
                 (cdr p2)
                 (iter (car p1)
                       (car p2)
                       frame)))
          (else
           FRAME-FAIL)))
  (iter p1 p2 frame))
(define (extend-if-possible var val frame)
  (let ((binding ((frame 'LOOKUP) var)))
    (cond (binding
           (unify-match
            (binding-value binding) val frame))
          ((var? val)                     ; {\em ; ***}
           (let ((binding ((frame 'LOOKUP) val)))
             (if binding
                 (unify-match
                  var (binding-value binding) frame)
                 ((frame 'EXTEND) var val 'I))))
          ((depends-on? val var frame)    ; {\em ; ***}
           FRAME-FAIL)
          (else ((frame 'EXTEND) var val 'J)))))
(define (depends-on? exp var frame)
  "occur check"
  (define (tree-walk e)
    (cond ((var? e)
           (if (equal? var e)
               true
               (let ((b ((frame 'LOOKUP) e)))
                 (if b
                     (tree-walk (binding-value b))
                     false))))
          ((pair? e)
           (or (tree-walk (car e))
               (tree-walk (cdr e))))
          (else false)))
  (tree-walk exp))

(define (instantiate exp frame unbound-var-handler)
  (define (copy exp)
    (cond ((var? exp)
           (let ((binding ((frame 'LOOKUP) exp)))
             (if binding
                 (copy (binding-value binding))
                 (unbound-var-handler exp))))
          ((pair? exp)
           (cons (copy (car exp)) (copy (cdr exp))))
          (else exp)))
  (copy exp))

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

;;; Debugging

(define O
  (lambda (k . x)
    "pretty format a query and indent it with spaces and points."
    (~a " ...\n"
        (foldr
         (lambda (x r)
           (string-append "   .   .   .   .   ." x "\n" r))
         ""
         (string-split (pretty-format k (if (null? x) 40 (car x)))
                       "\n"))
        " ...\n")))

(define ~a_
  (lambda (N)
    (lambda (x . trim)
      (let ((S (~a x #:width N)))
        (if (null? trim)
            S
            (string-trim S))))))
(define ~100 (~a_ 100))
(define ~70 (~a_ 70))
(define ~50 (~a_ 50))
(define ~40 (~a_ 40))
(define ~30 (~a_ 30))
(define ~20 (~a_ 20))
(define ~15 (~a_ 15))
(define ~10 (~a_ 10))
(define ~7  (~a_ 7))

(define (DBG/FRAMES frame-stream m)
  (o " >>> ")
  (S:for-each (lambda (x) (d (~a (x m) "\n\t~~*~~")))
              frame-stream)
  (d "<<<"))

(define (DBG/EVAL CALLER query frame-stream)
  (define head
    (~a ">"
        (make-string 30 #\~) "QUERY-EVAL" (make-string 30 #\~)
        "< [" "] "))
  (define foot
    (~a ">"
        (make-string 30 #\=) "QUERY-EVAL" (make-string 30 #\=)
        "< [" "] "))
  (d head ":" CALLER)
  (o (O query 50))
  (DBG/FRAMES frame-stream 'STR)
  (d foot)
  (d))

(define DBG/RULE
  (lambda (rule)
    (~a (~40 (conclusion rule))
        "=> "
        (rule-body rule))))

(define (DBG/RULES stream K pattern)
  (d "__APPLY-RULES:" pattern)
  (let ((k (char->integer #\A)))
    (for-each (lambda (x) (d (~7 (~a "~~" (begin
                                       (set! k (+ k 1))
                                       (integer->char (sub1 k)))
                                " "
                                ":" K))
                        (DBG/RULE x)))
              (S->list stream)))
  (d ".."))

(define DBG/APPLY
  (lambda (rule query-pattern right-vals fresh-list env)
    (define head (make-string 40 #\$))
    (d)
    (d)
    (d head)
    (d "--" 'PAT query-pattern)
    (d "--" 'R/V  right-vals)
    (d "--" 'FRE  fresh-list)
    (d "--" 'R/C (conclusion rule))
    (d "--" 'R/B (rule-body rule))
    (d "--" 'ENV (env 'STR))))

(define DBG/UNIF
  (lambda (LEFT RIGHT frame)
    (d ">>" (~50 LEFT)
       ">>>>" (~50 RIGHT 't)
       "\n"
       (make-string 100 #\~)
       "\n ##" (~100 (frame 'SHOW) 't))
    (d)))

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
                 (query/eval 'REPL q (S:singleton EMPTY-FRAME))))
               (query-driver-loop (cdr k)))))))

(define test/
  query-driver-loop)

(module+ test
  
;;; Genesis data base
;;; (rule (== ?x ?x))                         |     (son Adam Cain)
;;; (rule ((great ?rel . ?rest) ?x ?y)        |     (son Cain Enoch)
;;;       (and (== ?rel grandson)             |     (son Enoch Irad)
;;;            (== () ?rest)                  |     (son Irad Mehujael)
;;;            (son ?x ?w)                    |     (son Mehujael Methushael)
;;;            (grandson ?w ?y)))             |     (son Methushael Lamech)
;;; (rule ((great ?rel . ?rest) ?x ?y)        |     (wife Lamech Ada)
;;;       (and (== ?rel great)                |     (son Ada Jabal)
;;;            (son ?x ?u)                    |     (rule (grandson ?S ?G)
;;;            ((great . ?rest) ?u ?y)))      |           (and (son ?S ?F)
;;;                                           |                (son ?F ?G)))
;;;                                           |     (rule (son       ?M ?S)
;;;                                           |           (and (wife ?M ?W)
;;;                                           |                (son  ?W ?S)))

  
  (initialize-data-base
   (append genesis-data-base
           '((rule (== ?x ?x))
             (rule ((great ?rel . ?rest) ?x ?y)
                   (and (== ?rel grandson)
                        (== () ?rest)
                        (son ?x ?w)
                        (grandson ?w ?y)))

             (rule ((great ?rel . ?rest) ?x ?y)
                   (and (== ?rel great)
                        (son ?x ?u)
                        ((great . ?rest) ?u ?y))))))

  (test/ '(
           (COMM "APPLY won't refresh the procedure body but extends the environment.")
           
           (COMM "genesis")
           
           ((great grandson) Cain ?a)
           
           (grandson Cain ?a)
           
           (?relationship Adam Jabal) ))
  
  
  
  (initialize-data-base peano-data-base)
  
  (test/ '(
           (COMM "Peano")
           (add ?x ?y (S (S (S (S zero))))) ))
  
  (initialize-data-base microshaft-data-base)
  
  (test/ '(
           (COMM "micro shaft")
           
           (job ?x ?y)
           
           (job ?x (computer . ?k))

           (job ?x (computer ?k))
           
           (same ?a a)
           (same a ?a)
           (same ?a ?a)

           (unknown . ?a)
           
           (?all . ?x)

           (and (job ?x (computer . ?i))
                (address ?x ?a)) ))
  


  'done)

