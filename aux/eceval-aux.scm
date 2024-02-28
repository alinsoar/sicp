
;;;; the metacircular evaluator's syntax procedures from section 4.1.2


;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ SYNTAX

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))


(define quoted?
  (tagged-list? 'quote))

(define (text-of-quotation exp) (cadr exp))

(define variable? symbol?)

(define assignment? (tagged-list? 'set!))
(define assignment-variable cadr)
(define assignment-value caddr)


(define definition? (tagged-list? 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) (cddr exp))))

(define lambda? (tagged-list? 'lambda))
(define lambda-parameters cadr)
(define lambda-body cddr)

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define if? (tagged-list? 'if))
(define if-predicate cadr)
(define if-consequent caddr)
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))


(define begin? (tagged-list? 'begin))
(define begin-actions cdr)

(define (last-exp? seq) (null? (cdr seq)))
(define first-exp car)
(define rest-exps cdr)

(define application? pair?)
(define operator car)
(define operands cdr)

(define make-application cons)

(define no-operands? null?)
(define first-operand car)
(define rest-operands cdr)

;;;**following needed only to implement COND as derived expression,
;;; not needed by eceval machine in text.  But used by compiler

;; from 4.1.2
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define cond? (tagged-list? 'cond))
(define cond-clauses cdr)
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define cond-predicate car)
(define cond-actions cdr)

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

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

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 


;;;SECTION 4.1.3
;;; operations used by compiled code and eceval except as noted

(define (true? x) (not (eq? x false)))
;; * not used by eceval itself -- used by compiled code when that is run in the eceval machine
(define (false? x) (eq? x false))

;;following compound-procedure operations not used by compiled code
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define compound-procedure?
  (tagged-list? 'procedure))
(define procedure-parameters cadr)
(define procedure-body caddr)
(define procedure-environment cadddr)

(define enclosing-environment cdr)
(define first-frame car)
(define the-empty-environment '())
(define (make-frame variables values)
  (cons variables values))

(define frame-variables car)
(define frame-values cdr)

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))


(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
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
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
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
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))


;;; environment available for input code, not for machine simulator of e-c-evaluator.

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define primitive-procedure? (tagged-list? 'primitive))
(define primitive-implementation cadr)
;;; this is the list of procedures allowed to compose programs with.
(define primitive-procedures
  `( (car ,car)
     (cdr ,cdr)
     (cons ,cons)
     (null? ,null?)
     ;;above from book -- here are some more
     (+ ,+)
     (- ,-)
     (* ,*)
     (= ,=)
     (/ ,/)
     (> ,>)
     (< ,<)
     ;; other
     (display ,display)
     (newline ,newline)
     ))
(define (primitive-procedure-names)
  (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))
(define apply-in-underlying-scheme apply)
(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))


(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

;;; Simulation of new machine operations needed by eceval machine (not used by compiled code)

(define (empty-arglist) '())
(define (adjoin-arg arg arglist) (append arglist (list arg)))
(define (last-operand? ops) (null? (cdr ops)))

(define (no-more-exps? seq) (null? seq))

(define (get-global-environment) the-global-environment)

;;; Simulation of new machine operations needed for compiled code and eceval/compiler interface (not used by plain eceval machine)

(define (make-compiled-procedure entry env) (list 'compiled-procedure entry env))
(define compiled-procedure? (tagged-list? 'compiled-procedure))
(define compiled-procedure-entry cadr)
(define compiled-procedure-env caddr)

