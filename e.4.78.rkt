#lang racket

(require "sicp.rkt")
(GETMOD 4 1 without eval apply)
(require "e.4.55.S.rkt")
(require "e.4.55.DB.rkt")

(define eval
  (lambda (query)
    (case (type query)
      ('and          (conjoin      (contents query)))
      ('not          (negate       (contents query)))
      ('always-true  (always-true  (contents query)))
      (else (apply query)))))

(define apply
  (lambda (query-pattern)
    (lambda (frame succ fail)
      (find-assertions query-pattern
                       frame
                       succ
                       (lambda ()
                         (find-rules query-pattern
                                     frame
                                     succ
                                     fail))))))

(define conjoin
  (lambda (conjuncts)
    (lambda (frame succ fail)
      (if (empty-conjunction? conjuncts)
          (succ frame fail)
          ((eval (first-conjunct conjuncts))
           frame
           (lambda (v next)
             ((conjoin (rest-conjuncts conjuncts))
              v
              succ
              next))
           fail)))))

(define negate
  (lambda (query)
    (lambda (frame succ fail)
      ((eval (car query))
       frame
       (lambda (_ __)
         (fail))
       (lambda ()
         (succ frame fail))))))

(define (always-true _)
  (lambda (frame succ fail)
    (succ frame fail)))

;;; Unify assertions from the data base
(define (find-assertions pattern frame succ fail)
  (define next
    (lambda (data)
      (if (null? data)
          (fail)
          (let ((datum (car data))
                (next (lambda () (next (cdr data)))))
            (let ((match-result (unify-match pattern datum frame)))
              (if (match-result 'FAILED?)
                  (next)
                  (succ match-result
                        next)))))))
  (next
   (S->list ((DB 'fetch-assertions) pattern frame))))

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

;;; Unify combinations from rules
(define find-rules
  (lambda (pattern frame succ fail)
    (define next
      (lambda (rules)
        (if (null? rules)
            (fail)
            (let ((rule (car rules))
                  (next (lambda () (next (cdr rules)))))
              (apply-a-rule rule
                            pattern
                            frame
                            succ
                            next)))))
    (next
     (S->list ((DB 'fetch-rules) pattern frame)))))

(define (apply-a-rule rule query-pattern env succ fail)
  (let ((clean-rule (rename-variables-in rule)))
    (let ((unify-result
           (unify-match query-pattern
                        (conclusion clean-rule)
                        env)))
      (if (unify-result 'FAILED?)
          (fail)
          ((eval (rule-body clean-rule))
           unify-result
           succ
           fail)))))

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

(define rule-counter 0)
(define (new-rule-application-id)
  (set! rule-counter (+ 1 rule-counter))
  rule-counter)
(define (make-new-variable var rule-application-id)
  (cons '? (cons rule-application-id (cdr var))))

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

;;; Unification

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

;;; Syntax
(define (empty-conjunction? exps) (null? exps))
(define (first-conjunct exps) (car exps))
(define (rest-conjuncts exps) (cdr exps))

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
               ((eval q)
                EMPTY-FRAME
                (lambda (v next)
                  (d (instantiate q v (lambda (v) (contract-question-mark v))))
                  (next))
                (lambda ()
                  'fail))
               (query-driver-loop (cdr k)))))))

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

  (query-driver-loop
   '(
     (COMM "This is query evaluator with backtracking such as in Prolog.")
     
     (COMM "genesis")
     
     (== 5 5)
     
     (son ?x Cain)
     
     ((great grandson) Cain ?a)
     
     (grandson Cain ?a)
     
     (?relationship Adam Jabal)))
  
  (initialize-data-base peano-data-base)
  
  (query-driver-loop
   '(
     (COMM "Peano")
     (add ?x ?y (S (S (S (S zero))))) ))
  
  (initialize-data-base '((rule (=/= ?x ?y)
                                (not (== ?x ?y)))
                          (rule (== ?x ?x))))
  
  (query-driver-loop
   '(
     (COMM "disequality")
     (== 5 5)
     (== 5 6)
     (=/= 5 5)
     (=/= 5 6)
     ))
  
  'done)

