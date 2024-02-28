#lang racket

(require (prefix-in a: racket/base))
(require "sicp.rkt")
(GETMOD 4 1 without eval apply lambda-body lookup-variable-value)
(GETMOD 4 6 without eval)
(GETMOD 4 6 sequential:)
(GETMOD 4 16 simultaneous:)
(GETMOD 1 44)

;;;SECTION 4.1.1
(define (eval exp env)
  '(d "~" exp)
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
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))
(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(define (list-of-values-ORIG exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((leftmost-value (eval (first-operand exps) env)))
        (cons leftmost-value (list-of-values (rest-operands exps) env)))))
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))
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

;;; SET OPERATIONS
(define (belongs x s)
  (reduce (lambda (a acc) (or (equal? x a) acc))
          false
          s))
(define (merge . l)
  (a:apply append l))
(define (difference s1 s2)
  (filter (lambda (x) (not (belongs x s2)))
          s1))
(define (intersect s1 s2)
  (filter (lambda (x) (belongs x s2))
          s1))
(define (make-topological-sort variables constraints)
  (define graph
    (filter (lambda (x) (not (null? x)))
            (map (lambda (x) (a:apply append
                                 (map (lambda (y) (cons y (car x)))
                                      (cdr x))))
          constraints)))
  (define (edges-from-n n graph)
    ; (d "->" n graph)
    (filter (lambda (x) (eq? (car x) n))
            graph))
  (define (remove-from-graph edges graph)
    (difference graph edges))
  (define (no-incoming-edge-nodes graph)
    (difference variables (map cdr graph)))
  (define (empty-entries ms graph)
    (intersect ms (no-incoming-edge-nodes graph)))
  (define (add-nodes-to-S ms graph)
    "take the nodes from MS that have no incoming edges in GRAPH."
    (let ((out-nodes (map cdr ms)))
          (intersect (no-incoming-edge-nodes graph)
                     (map cdr ms))))
  (define (loop S L graph)
    "topological sort. S will keep all the time the nodes that have no
incoming nodes in the graph, and L will keep invariant the nodes that
have already been sorted."
    (cond ((null? S)
           (if (null? graph)
               L
               nil))
          (else (let* ((n (car S))      ; remove a node N of S
                       (S (cdr S))
                       (L (append L (list n))) ; put N at the tail of L
                       (ms (edges-from-n n graph)) ; remove edges (N,_) from graph
                       (graph (remove-from-graph ms graph))
                       (S (append S (add-nodes-to-S ms graph))))
                  (loop S L graph)))))
  (loop (no-incoming-edge-nodes graph) '() graph))

;;; ORDERED INITIALIZATION
(define (make-ordered-initialization assignments)
  (define (intersect s1 s2)
    (filter (lambda (x) (belongs x s2)) s1))
  ;; (rp 10 "---") (d)
  (let* ((variables (map assignment-variable assignments))
         (ordered-initialization
          (make-topological-sort
           variables
           (map (lambda (exp) (cons (assignment-variable exp)
                               (intersect
                                (free-variables (assignment-value exp))
                                variables)))
                assignments))))
    (if (null? ordered-initialization)
        ;; this will generate an error of *unassigned variable*
        assignments
        ;; this will reorder the initializations, to make it possible
        (map (lambda (v) (cons 'set!
                          (cons v
                                (cddar (filter (lambda (a) (eq? (cadr a) v))
                                               assignments)))))
             ordered-initialization))))
(define (lambda-body exp)
  "Expand lambda when the lambda is defined."
  (let ((body (cddr exp)))
    (let ((lambda-data
           (scan-out-defines
            body
            (lambda (vars set-instr instructions)
              (if (eq? vars '())
                  body
                  (list
                   (let-apply-lambda
                    vars
                    (append (make-ordered-initialization set-instr)
                            instructions)
                    (map (lambda (_) ''*unassigned*) vars))))))))
      lambda-data)))
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (mcar vars))
             (if (eq? (mcar vals) '*unassigned*)
                 (error "Unassigned variable --" (mcar vars))
                 (mcar vals)))
            (else (scan (mcdr vars) (mcdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))
(define (scan-out-defines e co)
  (cond ((null? e) (co '() '() '()))
        ((definition? (car e))
         (scan-out-defines
          (cdr e)
          (lambda (vars set-instr instr)
            (co (cons (definition-variable (car e)) vars)
                (cons (cons 'set!
                            (list (definition-variable (car e))
                                  (definition-value (car e))))
                      set-instr)
                instr))))
        (else (scan-out-defines
               (cdr e)
               (lambda (vars set-instr instr)
                 (co vars set-instr (cons (car e) instr)))))))

(define (free-variables expression)
  "compute the free-variables for `EXPRESSION`"
  (define (recseq seq bound)
    (a:apply merge (map (lambda (x) (rec x bound)) seq)))
  (define (rec exp bound)
    (difference
     (cond
      ((self-evaluating? exp) '())
      ((variable? exp)        (list exp))
      ((quoted? exp)          '())
      ((assignment? exp)      (rec (assignment-value exp)
                                   bound))
      ((definition? exp)      (rec (definition-value exp)
                                   (cons (definition-variable exp) bound)))
      ((if? exp)              (merge (rec (if-predicate exp) bound)
                                     (rec (if-consequent exp) bound)
                                     (rec (if-alternative exp) bound)))
      ((lambda? exp)          (recseq (lambda-body exp)
                                      (merge (lambda-parameters exp) bound)))
      ((begin? exp)           (recseq (begin-actions exp) bound))
      ((cond? exp)            (rec (cond->if exp) bound))
      ((application? exp)     (merge (rec (operator exp) bound)
                                     (recseq (operands exp) bound)))
      (else
       (error "Unknown expression type -- FREEVAR" exp)))
     bound))
  (rec expression '()))

(module+ test
  (define (test-eval-seq e) (sequential:eval e the-global-environment))
  (define (test-eval-sim e) (simultaneous:eval e the-global-environment))
  (define (test-eval-make-constraint-bindings e) (eval e the-global-environment))
  (define (test evaluator)
    (d (with-handlers (((lambda (v) (exn? v)) (lambda (v) (exn-message v)))
                       ((lambda (_) true) (lambda (_) (error "WILL NOT HAPPEN"))))
         (evaluator '((lambda (a)
                        (define (f x)
                          (define b (+ a x))
                          (define a 5)
                          (+ a b))
                        (f 10))
                      1)))))
  "BEN: SEQUENTIAL EXECUTION"
  (test test-eval-seq)
  "ALYSSA: SIMULTANEOUS EXECUTION"
  (test test-eval-sim)
  "-- FREE VARIABLES COMPUTATION --"
  "for a variable"
  (free-variables 'a)
  "for a quotation"
  (free-variables ''a)
  (free-variables ''(+ 1 2 3 a))
  "for a self-evaluating expression"
  (free-variables 5)
  "for an assignment"
  (free-variables '(set! a b))
  (free-variables '(set! a 3))
  "for a definition"
  (free-variables '(define x x))
  (free-variables '(define x y))
  "for lambdas"
  (free-variables '(lambda (x) y))
  "for application"
  (free-variables '(+ a b c 2))
  (free-variables '(+ a x))
  (free-variables '((lambda (a b) a b c 2)
                    w y))
  (free-variables '((lambda (a b c) a b c 2)
                    a b c))
  (free-variables '((lambda (a b c) a b c 2)
                    1 'nil nil))
  (free-variables '((lambda (a b) a b c 2)
                    w (+ 2 3 4 a b c)))
  "for begin sequence"
  (free-variables '(begin a b c (+ a b w)))
  "for if"
  (free-variables '(if a 2 (+ w (* q u))))
  "for conditional statement"
  (free-variables '(cond (true 2)
                    ((eq? 3 4) a)
                    ((= w 1) +)
                    (else (* a b (+ c 2)))))
  "EVA: INTELLIGENT BUT SLOW BINDING"
  (test test-eval-make-constraint-bindings)
  (with-handlers
      (((lambda (v) (exn? v))
        (lambda (_) "impossible to sort the initializations topologically")))
    (test-eval-make-constraint-bindings '((lambda (a)
                                            (define (f x)
                                              (define b (+ a x))
                                              (define a (+ b x))
                                              (+ a b))
                                            (f 10))
                                          1)))
  'done)

