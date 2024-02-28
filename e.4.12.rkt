#lang racket

(require "sicp.rkt")
(require scheme/mpair)
(require compatibility/mlist)
(GETMOD 4 1 without
        eval apply
        extend-environment
        lookup-variable-value
        set-variable-value!
        define-variable!
        print-environment
        the-global-environment
         the-empty-environment
        enclosing-environment
        first-frame
        frame-values
        frame-variables
        add-binding-to-frame!)

;;;SECTION 4.1.1 -- LINK
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
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
;;; LINK EVAL-TOOLS
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
;;; LINK FRAMES
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
(define (print-environment env)
  (define (iter env N)
    (let ((ee (enclosing-environment env)))
      (rp (* N 4) " ")
      (cond ((eq? ee the-empty-environment) (d "GLOBAL ENVIRONMENT"))
            ((> N 0) (d "PARENT ENVIRONMENT"))
            (else (d "ENVIRONMENT")))
      (define (iter-frame var exp)
        (if (null? var)
            'ok
            (begin (rp (* 4 N) " ")
                   (d (mcar exp) "=>"
                      (cond ((compound-procedure? (mcar var))
                             (format "compound-procedure ~a ~a ~a"
                                     (procedure-parameters (mcar var))
                                     (procedure-body (mcar var))
                                     (frame-variables
                                      (first-frame
                                       (procedure-environment
                                        (mcar var))))))
                            (else (mcar var))))
                   (iter-frame (mcdr var) (mcdr exp)))))
      (iter-frame
       (frame-values (first-frame env))
       (frame-variables (first-frame env)))
      (if (eq? ee the-empty-environment)
          'ok
          (iter ee (+ N 1)))))
  (iter env 0))

;;; ABSTRACT SCANNING OF THE ENVIRONMENT
(define (scan-environment vars vals var action-not-match action-match)
  (cond ((null? vars) (action-not-match))
        ((eq? var (mcar vars)) (action-match vals))
        (else (scan-environment (mcdr vars) (mcdr vals)
                                var action-not-match action-match))))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (scan-environment (frame-variables frame)
                      (frame-values frame)
                      var
                      (lambda () (add-binding-to-frame! var val frame))
                      (lambda (vals) (set-mcar! vals val)))))
(define (lookup-variable-value var env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan-environment (frame-variables frame)
                            (frame-values frame)
                            var
                            (lambda () (env-loop (enclosing-environment env)))
                            (lambda (vals) (mcar vals))))))
  (env-loop env))
(define (set-variable-value! var val env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan-environment (frame-variables frame)
                            (frame-values frame)
                            var
                            (lambda () (env-loop (enclosing-environment env)))
                            (lambda (vals) (set-mcar! vals val))))))
  (env-loop env))

;;; LINK
(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))
(define the-global-environment (setup-environment))

(module+ test
  (define (test-eval e) (eval e the-global-environment))
  (test-eval '(+ 1 2 3))
  (test-eval '(if (< 3 5) 1 2))
  (test-eval '(if (< 5 3) 1 2))
  (test-eval '(if (< 5 3) 'false (cons 'a (cons (+ 3 5) '()))) )
  (test-eval '(if (< 3 5) 'true 'false) )
  (test-eval '(((lambda (length)
                  (length length))
                (lambda (length)
                  (lambda (l)
                    (cond ((null? l) 0)
                          (else (+ 1 ((length length)
                                      (cdr l))))))))
               '(1 2 3)))
  (test-eval '(((lambda (length)
                  (length length))
                (lambda (length)
                  ((lambda (length)
                     (lambda (l)
                       (cond ((null? l) 0)
                             (else (+ 1 (length (cdr l)))))))
                   (lambda (x) ((length length) x)))))
               '(1 2 3)))
  (test-eval '(((lambda (length)
                  (length length))
                (lambda (length0)
                  ((lambda (length)
                     (lambda (l)
                       (cond ((null? l) 0)
                             (else (+ 1 (length (cdr l)))))))
                   (lambda (x) ((length0 length0) x)))))
               '(1 2 3)))
  (test-eval '((lambda (f)
                 (((lambda (length)
                     (length length))
                   (lambda (length0)
                     (f
                      (lambda (x) ((length0 length0) x)))))
                  '(1 2 3)))
               (lambda (length)
                 (lambda (l)
                   (cond ((null? l) 0)
                         (else (+ 1 (length (cdr l)))))))))
  (test-eval '(begin (define p ((lambda (f)
                                  ((lambda (length)
                                     (length length))
                                   (lambda (length0)
                                     (f
                                      (lambda (x) ((length0 length0) x))))))
                                (lambda (length)
                                  (lambda (l)
                                    (cond ((null? l) 0)
                                          (else (+ 1 (length (cdr l)))))))))
               (cons (car p) (cons (car (cdr p)) '()))))
  (test-eval '(((lambda (f)
                  ((lambda (rec) (rec rec))
                   (lambda (rec) (f (lambda (x) ((rec rec) x))))))
                (lambda (length)
                  (lambda (l)
                    (cond ((null? l) 0)
                          (else (+ 1 (length (cdr l))))))))
               '(1 2 3 4 5 6 7 8 9 0)))
  'done)


