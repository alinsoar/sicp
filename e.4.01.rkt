#lang racket

(require "sicp.rkt")
(require scheme/mpair)
(require compatibility/mlist)
(require (prefix-in a: racket/base))
(GETMOD 1 24)

;;;from section 4.1.4 -- must precede def of metacircular apply
(define apply-in-underlying-scheme a:apply)

;;;SECTION 4.1.1
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
          (~a "Unknown procedure type -- APPLY\n"
              " PROCEDURE:" procedure
              (foldl string-append
                     "\n--- STOP"
                     (map (lambda (a) (~a " \n ARG:" a "")) arguments)))))))

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

;;;SECTION 4.1.2
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (quoted? exp)
  (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (variable? exp) (symbol? exp))

(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (make-begin seq) (cons 'begin seq))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

;;;SECTION 4.1.3
(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define (make-frame variables values)
  (mcons variables values))
(define (frame-variables frame) (mcar frame))
(define (frame-values frame) (mcdr frame))
(define the-empty-environment '())
(define (add-binding-to-frame! var val frame)
  (set-mcar! frame (mcons var (mcar frame)))
  (set-mcdr! frame (mcons val (mcdr frame))))
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame (list->mlist vars) (list->mlist vals)) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (mcar vars))
             (mcar vals))
            (else (scan (mcdr vars) (mcdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (mcar vars))
             (set-mcar! vals val))
            (else (scan (mcdr vars) (mcdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))
(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (mcar vars))
             (set-mcar! vals val))
            (else (scan (mcdr vars) (mcdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))
(define (get-environment? exp)
  (and (quoted? exp)
       (eq? (text-of-quotation exp) 'env)))
(define (print-environment env . vars)
  (define (indent N newline?)
    (and newline? (d))
    (rp (* N 4) " "))
  (define (match name)
    (define (iter v)
      (cond ((null? v) false)
            ((eq? (car v) name) true)
            (else (iter (cdr v)))))
    (let ((vars (cond ((null? vars) vars)
                      ((pair? (car vars)) (car vars))
                      (else vars))))
     (or (null? vars)
         (iter vars))))
  (define (iter env N)
    (let ((ee (enclosing-environment env)))
      (indent N true)
      (o "*** " (cond ((eq? ee the-empty-environment) "GLOBAL ")
                      ((> N 0) "PARENT ")
                      (else ""))
         "ENVIRONMENT ***\n")
      (define (iter-frame var exp prev-newline)
        (cond ((null? var) 'ok)
              ((match (mcar exp))
               (indent N (if prev-newline false true))
               (d (mcar exp) "=>" (user-print (mcar var)))
               (iter-frame (mcdr var) (mcdr exp) true))
              (else
               (indent N false)
               (o " *FLUSHED*/" (mcar exp) "; " )
               (iter-frame (mcdr var) (mcdr exp) false))))
      (iter-frame
       (frame-values (first-frame env))
       (frame-variables (first-frame env))
       true)
      (if (eq? ee the-empty-environment)
          'ok
          (iter ee (+ N 1)))))
  (iter env 0))

;;;SECTION 4.1.4
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'list list)
        (list 'not not)
        (list 'eq? eq?)
        (list 'equal? equal?)
        (list 'pair? pair?)
        (list '+ +)
        (list '- -)
        (list '= =)
        (list '* *)
        (list '% remainder)
        (list '/ quotient)
        (list '0? zero?)
        (list '< <)
        (list '> >)
        (list 'abs abs)
        (list '<= <=)
        (list '>= >=)
        (list 'sqrt sqrt)
        (list 'integer? integer?)
        (list 'print-environment print-environment)
        (list 'time current-milliseconds)
        (list 'println d)
        (list 'display d)
        (list 'print o)
        (list 'newline newline)
        (list 'take take)
        (list 'drop drop)
        (list 'm2l mlist->list)
        (list 'mcar mcar)
        (list 'mcdr mcdr)
        (list 'mpair? mpair?)
        (list 'mlist mlist)
        (list 'symbol? symbol?)
        (list 'prime? (lambda (n) (fast-prime? n 10)))
        (list 'symbol-name symbol->string)
        (list 'string<? string<?)
        (list 'string>? string>?)
        (list 'true true)
        (list 'false false)
        (list 'even? even?)
        (list 'member member)
        ))
(define (primitive-procedure-names)
  (map car
       primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))
(define the-global-environment (setup-environment))

;[moved to start of file] (define apply-in-underlying-scheme apply)
(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

;;; REPL
(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))
(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))
(define (announce-output string)
  (newline) (display string) (newline))
(define (user-print object)
  (cond ((compound-procedure? object)
         (format "compound-procedure <~a> <~a> <~a>"
                 (procedure-parameters object)
                 (procedure-body object)
                 (frame-variables
                  (first-frame
                   (procedure-environment
                    object)))))
        (else object)))

(module+ test
  (define (test-eval e) (eval e the-global-environment))
  (test-eval '(+ 1 2 3))
  (test-eval '(if (< 3 5) 1 2))
  (test-eval '(if (< 5 3) 1 2))
  (test-eval '(if (< 5 3) 'false (cons 'a (cons (+ 3 5) '()))) )
  (test-eval '(if (< 3 5) 'true 'false) )
  (test-eval '(define (abs x)
                ((if (< 0 x)
                     +
                     -)
                 x)))
  (test-eval '(abs 111))
  (test-eval '(abs -111))

  (test-eval
   '((lambda (f ls)
       ((lambda (s)
          (s s ls))
        (lambda (map l)
          (if (null? l)
              '()
              (cons (f (car l))
                    (map map (cdr l)))))))
     (lambda (x) (+ x 1))
     '(1 2 3)))
  
  (test-eval
   '((lambda (f ls)
       ((lambda (s)
          (s s ls))
        (lambda (map l)
          (if (null? l)
              '()
              (cons (f (car l))
                    (map map (cdr l)))))))
     symbol-name
     '(a b c)))

  (test-eval '((lambda (f) (f 'a))
               symbol-name))

  (test-eval '((lambda (p1 p2 map)
                 ((lambda (s)
                    (s s
                       (map symbol-name p1)
                       (map symbol-name p2)))
                  (lambda (s n1 n2)
                    (cond ((null? n1) true)
                          ((null? n2) false)
                          ((string<? (car n1) (car n2)) true)
                          ((string>? (car n1) (car n2)) false)
                          (else (s s (cdr n1) (cdr n2)))))))
               '(a b c)
               '(d e f)
               (lambda (f ls)
                 ((lambda (f)
                    (f f ls))
                  (lambda (m l)
                    (if (null? l)
                        '()
                        (cons (f (car l))
                              (m m (cdr l)))))))))
  (test-eval
   '((lambda (p1 p2 map)
       ((lambda (s)
          (s s
             (map symbol-name p1)
             (map symbol-name p2)))
        (lambda (s n1 n2)
          (cond ((null? n1) true)
                ((null? n2) false)
                ((string<? (car n1) (car n2)) true)
                ((string>? (car n1) (car n2)) false)
                (else (s s (cdr n1) (cdr n2)))))))
     '(d e f)
     '(a b c)
     (lambda (f ls)
       ((lambda (f)
          (f f ls))
        (lambda (m l)
          (if (null? l)
              '()
              (cons (f (car l))
                    (m m (cdr l)))))))))

  'done)

(module+ export
  (provide eval
           apply
           the-global-environment
           self-evaluating?
           variable?
           lookup-variable-value
           quoted?
           text-of-quotation
           assignment?
           definition?
           if?
           lambda?
           make-procedure
           lambda-parameters
           lambda-body
           begin?
           begin-actions
           cond?
           cond->if
           application?
           operator
           operands
           primitive-procedure?
           apply-primitive-procedure
           compound-procedure?
           procedure-body
           extend-environment
           procedure-parameters
           procedure-environment
           no-operands?
           first-operand
           rest-operands
           true?
           if-predicate
           if-consequent
           if-alternative
           last-exp?
           first-exp
           rest-exps
           set-variable-value!
           assignment-variable
           assignment-value
           define-variable!
           definition-variable
           definition-value
           tagged-list?
           make-if
           make-begin
           sequence->exp
           make-lambda
           print-environment
           primitive-procedure-objects
           primitive-procedure-names
           frame-variables
           frame-values
           first-frame
           enclosing-environment
           the-empty-environment
           the-global-environment
           get-environment?
           user-print
           add-binding-to-frame!
           prompt-for-input
           announce-output
           prompt-for-input
           ))

