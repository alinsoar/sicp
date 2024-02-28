#lang racket                            ; -*- racket -*-

;;; Evaluator using the substitution model.

(require "sicp.rkt")

(define DEBUG/SUBST false)

(require (only-in racket/base [apply apply/scheme/internal]))

(define imported-primitives
  `((+ . ,+)
    (- . ,-)
    (* . ,*)
    (/ . ,/)
    (= . ,=)
    (> . ,>)
    (< . ,<)))
(define global-definitions
  '((nil . '())))
(define initial-environment
  (append imported-primitives global-definitions))

(define eval
  (lambda (expr bindings update yield)
    "Compute the value of EXPR by mutating the vector EXPR.  We use
UPDATE only for debugging purposes.  We suppose the code from EXPR to
be in kernel syntax, so to have been desugared before evaluation."
    ;; (d "EVAL:" expr)
    (cond
     ((self-eval? expr)   (yield expr bindings))
     ((quote? expr)       (yield (quote-expr expr) bindings))
     ((variable? expr)    (lookup expr bindings update yield))
     ((define? expr)      (eval-define expr bindings update yield))
     ((if? expr)          (eval-conditional expr bindings update yield))
     ((begin? expr)       (eval-sequence (vector-drop expr 1) bindings update yield))
     ((lambda? expr)      (eval-lambda expr bindings update yield))
     (else                (eval-combination expr bindings update yield)))))

(define apply
  (lambda (operator operands update bindings yield)
    ;; (d "APPLY:" operator " " operands)
    ;; (d "ENV:" bindings)
    (cond ((primitive? operator)        ; SCHEME INTERNAL
           (yield
            (apply/scheme/internal operator (vector->list operands))))
          (else                         ; LAMBDA
           (eval-sequence
            (lambda-body operator)
            (append (vector->list (vector-map cons
                                              (lambda-names operator)
                                              operands))
                    bindings)
            update
            (lambda (v b)
              (yield v)))))))

(define substitution
  (lambda (expr)
    (define code (l2v (desugar expr)))
    
    (define update
      (lambda (expr pos val)
        (define refresh?
          (and DEBUG/SUBST
               (not (equal? (vector-ref expr pos) val))))
        (vector-set! expr pos val)
        (and DEBUG/SUBST
             refresh?
             (d "UPDATE:" code))))
    (eval code
          initial-environment
          update
          (lambda (x _) x))))

(define lookup
  (lambda (name bindings update y)
    (let ((v (assoc name bindings)))
      (if v (eval (vector/deep/copy (cdr v)) bindings update y)
          (error "unknown variable" name)))))

(define eval-define
  (lambda (expr bindings update y)
    ;; (d "DEFINE:" expr)
    (y expr
       (cons (cons (define-name expr)
                   ;; we take care not to mutate the environment and
                   ;; make a deep copy
                   (vector/deep/copy (define-binding expr)))
             bindings))))

(define eval-combination
  (lambda (expr bindings update yield)
    ;; (d "COMBINATION:" expr)
    ;; (d "ENV:" bindings)
    
    ;; compute the operator and the operands
    (eval (operator expr)
          bindings
          update
          (lambda (op _)
            (update expr 0 op)
            ((lambda (s) (s s 1))
              (lambda (s idx)
                (if (valid-operand? expr idx)
                    (eval (sequence-nth expr idx)
                          bindings
                          update
                          (lambda (val _)
                            (update expr idx val)
                            (s s (add1 idx))))
                    'OK)))))
    
    (apply (operator expr)
           (operands expr)
           update
           bindings
           (lambda (val)
             (yield val bindings)))))

(define update-name
  (lambda (expr oldname freshname update s f)
    ;; (d "\nUPDATE FRESH" expr)
    ;; (d oldname " ==> " freshname "\n" f)
    (cond ((self-eval? expr)
           (f))
          ((quote? expr)
           (update-name (quote-expr expr)
                        oldname
                        freshname
                        update
                        (lambda (v)
                          (update expr 1 v)
                          (f))
                        f))
          ((variable? expr)
           (if (eq? expr oldname)
               (s freshname)
               (f)))
          ((define? expr)
           
           (update-name (define-name expr)
                        oldname
                        freshname
                        update
                        (lambda (v)
                          (update expr 1 v)
                          (f))
                        f)
           
           (update-name (define-binding expr)
                        oldname
                        freshname
                        update
                        (lambda (v)
                          (update expr 2 v)
                          (f))
                        f))
          ((lambda? expr)
           (if (vector-memq oldname (lambda-names expr))
               (f)
               (let ((a 'a))
                 (define (iter idx)
                   (if (valid-operand? expr idx)
                       (update-name (vector-ref expr idx)
                                    oldname
                                    freshname
                                    update
                                    (lambda (v)
                                      (update expr idx v)
                                      (iter (add1 idx)))
                                    (lambda ()
                                      (iter (add1 idx))))
                       (f)))
                 (iter 2))))
          ((combination? expr)
           (define (iter idx)
             (if (valid-operand? expr idx)
                 (update-name (vector-ref expr idx)
                              oldname
                              freshname
                              update
                              (lambda (v)
                                (update expr idx v)
                                (iter (add1 idx)))
                              (lambda ()
                                (iter (add1 idx))))
                 (f)))
           (iter 0))
          (else (error "update-name:" expr)))
    expr))

(define refresh-sequence-names
  (lambda (seq name freshname idx update)
    (and (sequence-rest? seq (sub1 idx))
         (update-name (vector-ref seq idx)
                      name
                      freshname
                      update
                      (lambda (v)
                        (update seq idx v)
                        'ok)
                      (lambda ()
                        'ok))
         (refresh-sequence-names seq name freshname (add1 idx) update))))

(define eval-sequence
  (lambda (expr bindings update yield)
    "we suppose there is at least one expression in sequence."
    ;; (d "SEQ:" expr)
    ;; (d "ENV:" bindings)
    
    ;; Collect the definitions and made fresh names
    ((lambda (s) (s s 0))
     (lambda (s idx)
       (define EL (sequence-nth expr idx))
       (and (define? EL)
            (let ((name (define-name EL)))
              (refresh-sequence-names expr name (fresh name) 0 update)))
       (and (sequence-rest? expr idx)
            (s s (add1 idx)))))
    
    ;; Evaluate each expression after the names defined in the current
    ;; sequence have been refreshed.
    ((lambda (s) (s s 0 bindings))
     (lambda (s idx bindings*)
       (eval (sequence-nth expr idx)
             bindings*
             update
             (lambda (val bindings**)
               (update expr idx val)
               (if (sequence-rest? expr idx)
                   (s s (add1 idx) bindings**)
                   (yield val bindings**))))))))

(define eval-conditional
  (lambda (expr bindings update y)
    (eval (if-cond expr)
          bindings
          update
          (lambda (testval bindings*)
            (eval ((if testval if-then if-else)
                   expr)
                  bindings*
                  update
                  y)))))

(define eval-lambda
  (lambda (expr bindings update yield)
    (define names (lambda-names expr))
    (define freshnames (vector-map fresh names))
    (update expr 1 freshnames)
    (vector-map (lambda (name freshname)
                  (refresh-sequence-names expr
                                          name
                                          freshname
                                          2
                                          update))
                names
                freshnames)
    (yield expr bindings)))

;;; `````````````````````````````````````````````````` KERNEL SYNTAX

(define (tagged? tag)
  (lambda (expr)
    (and (vector? expr)
         (eq? tag (vector-ref expr 0)))))

(define (self-eval? expr)
  (or (number? expr)
      (boolean? expr)
      (list? expr)
      (procedure? expr)))

(define (lambda? expr)
  ((tagged? 'lambda) expr))
(define (lambda-names expr)
  (vector-ref expr 1))
(define (lambda-body expr)
  (vector-drop expr 2))

(define (quote? expr)
  ((tagged? 'quote) expr))
(define (quote-expr expr)
  (vector-ref expr 1))

(define (variable? expr)
  (symbol? expr))

(define (combination? expr)
  (vector? expr))
(define (valid-operand? expr idx)
  (> (vector-length expr) idx))
(define (operator expr)
  (vector-ref expr 0))
(define (operands expr)
  (vector-drop expr 1))

(define (define? expr)
  ((tagged? 'define) expr))
(define (define-name expr)
  (vector-ref expr 1))
(define (define-binding expr)
  (vector-ref expr 2))

(define (begin? expr)
  ((tagged? 'begin) expr))
(define (sequence-nth expr idx)
  (vector-ref expr idx))
(define (sequence-rest? expr idx)
  (> (vector-length expr) (add1 idx)))

(define (if? expr)
  ((tagged? 'if) expr))
(define (if-cond expr)
  (vector-ref expr 1))
(define (if-then expr)
  (vector-ref expr 2))
(define (if-else expr)
  (vector-ref expr 3))

;;; `````````````````````````````````````````````````` SYNTACTIC SUGAR

(define desugar
  (lambda (expr)
    ;; (d "DESUGAR:" expr)

    (define (tagged? tag)
      (and (list? expr)
           (eq? tag (car expr))))
    
    (cond ((tagged? 'begin)
           (cons 'begin (map desugar (cdr expr))))
          ((tagged? 'define)
           (cons 'define (cons (cadr expr) (map desugar (cddr expr)))))
          ((tagged? 'lambda)
           (cons 'lambda (cons (cadr expr) (map desugar (cddr expr)))))
          ((tagged? 'if)
           (cons 'if (map desugar (cdr expr))))
          ((tagged? 'quote)
           (cons 'quote (map desugar (cdr expr))))
          ((tagged? 'cond)
           (foldr (lambda (e r)
                    (cond ((eq? 'else (car e))
                           (cons 'begin (cdr e)))
                          (else
                           (list 'if
                                 (car e)
                                 (cons 'begin (cdr e))
                                 (if (null? r)
                                     ''undefined
                                     r)))))
                  '()
                  (cdr expr)))
          ((tagged? 'and)
           (foldr (lambda (e r)
                    (list 'if
                          e
                          r
                          false))
                  true
                  (cdr expr)))
          ((list? expr)
           (map desugar expr))
          (else expr))))

;;; `````````````````````````````````````````````````` FRESH

(define fresh
  (lambda (var)
    (define counter
      (let ((counters '()))
        (define counter/get!
          (lambda (id)
            (or (symbol? id)
                (error "counter/get"))
            (if (assoc id counters)
                (cdr (assoc id counters))
                (begin
                  (set! counters (cons (cons id -1) counters))
                  -1))))
        (define counter/set!
          (lambda (id newval)
            (or (symbol? id)
                (error "counter/set!"))
            (define (refresh vals)
              (cond ((null? vals)         (list (cons id newval)))
                    ((eq? (caar vals) id) (cons (cons id newval) (cdr vals)))
                    (else                 (cons (car vals) (refresh (cdr vals))))))
            (set! counters (refresh counters))))
        (define (1+ id oldval)
          (counter/set! id (+ 1 oldval))
          (counter/get id))
        (lambda (perm)
          (perm counter/get! 1+))))
    (define counter/get
      (counter (lambda (get _) get)))
    (define counter/1+
      (counter (lambda (_ 1+) 1+)))

    (define get-index
      (lambda (var-string n)
        (let ((next (string-ref var-string n)))
          (cond ((char-numeric? next) (get-index var-string (- n 1)))
                ((char=? next #\#)    (+ 1 n))
                (else                 (string-length var-string))))))
    (let ((var-string (symbol->string var)))
      (let ((lngth (string-length var-string)))
        (let ((pos (get-index var-string (- lngth 1))))
          (let ((name/str (substring var-string 0 (if (= pos lngth) pos (- pos 1)))))
            (let ((index (string->number (substring var-string pos lngth))))
              (let ((name (string->symbol name/str)))
                (let ((index (or index (counter/get name))))
                  (let ((new/index (counter/1+ name index)))
                    (string->symbol
                     (string-append
                      name/str
                      "#"
                      (number->string new/index)))))))))))))

;;; `````````````````````````````````````````````````` TOOLS

(define deep-copy
  (lambda (p? op map)
    (lambda (l)
      ((lambda (s) (s s l))
       (lambda (s expr)
         (cond ((p? expr)
                (op (map (lambda (e) (s s e)) expr)))
               (else
                expr)))))))
(define l2v
  (deep-copy list? list->vector map))
(define v2l
  (deep-copy vector? vector->list vector-map))
(define list/deep/copy
  (lambda (v) (v2l (l2v v))))
(define vector/deep/copy
  (lambda (v) (l2v (v2l v))))

;;; `````````````````````````````````````````````````` TEST

(module+ test
  (substitution
   '(begin
     (define nn 200)
     (define fact
       (lambda (n)
         (if (= n 0)
             1
             (* n (fact (- n 1))))))
     
     10
     30
     nil
     (define fact1
       (lambda (n)
         (if (= n 1)
             1
             (+ n
                (if (= (- n 1) 1)
                    1
                    (+ (- n 1)
                       (if (= (- n 2) 1)
                           1
                           1)))))))
     (+ 5 3)
     (- 9 1)
     (/ 6 2)
     (+ (* 2 4) (- 4 6))
     (+ 1 2 3 )
     (define a 3)
     (define b (+ a 1))
     (+ a b (* a b))
     (= a b)
     (if (and (> b a) (< b (* a b)))
         b
         a)
     (and (> b a) (< b (* a b)))
     
     (cond ((= a 4) 6)
           ((= b 4) (+ 6 7 a))
           (else 25))
     (+ 2 (if (> b a) b a))
     (* (cond ((> a b) a)
              ((< a b) b)
              (else -1))
        (+ a 1))
     (fact nn)
     
     )))


