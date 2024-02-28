
;;; the explicit control evaluator

;;; this lazy evaluator is NOT tail recursive. To make it tail
;;; recursive, before the evaluation of the last argument of an
;;; application we should not save neither the unev nor the env
;;; registers.


;; Lazy evaluation combined with memoization = __call-by-need___ argument passing
;; Lazy evaluation without memoization = __call-by-name___ argument passing

(load "aux/msim")

(load "aux/eceval-aux")

(set! make-stack make-stack2)

(define thunk? (tagged-list? 'thunk))
(define make-thunk
  (lambda (exp env)
    (if (thunk? exp)
        exp
        (list 'thunk exp env))))
(define thunk-exp cadr)
(define thunk-env caddr)

(define evaluated-thunk? (tagged-list? 'evaluated-thunk))
(define thunk-set-value! (lambda (thunk val)
                           (set-car! thunk 'evaluated-thunk)
                           (set-car! (cdr thunk) '__)
                           (set-car! (cddr thunk) val)))
(define thunk-value caddr)

(define eceval-operations
  `(
    ;;                    primitive Scheme operations
    (read ,read)
    ;;                    operations in syntax.scm
    (self-evaluating? ,self-evaluating?)
    (quoted? ,quoted?)
    (text-of-quotation ,text-of-quotation)
    (variable? ,variable?)
    (assignment? ,assignment?)
    (assignment-variable ,assignment-variable)
    (assignment-value ,assignment-value)
    (definition? ,definition?)
    (definition-variable ,definition-variable)
    (definition-value ,definition-value)
    (lambda? ,lambda?)
    (lambda-parameters ,lambda-parameters)
    (lambda-body ,lambda-body)
    (if? ,if?)
    (if-predicate ,if-predicate)
    (if-consequent ,if-consequent)
    (if-alternative ,if-alternative)
    (begin? ,begin?)
    (begin-actions ,begin-actions)
    (last-exp? ,last-exp?)
    (first-exp ,first-exp)
    (rest-exps ,rest-exps)
    (application? ,application?)
    (operator ,operator)
    (operands ,operands)
    (no-operands? ,no-operands?)
    (first-operand ,first-operand)
    (rest-operands ,rest-operands)
    ;;                    operations in eceval-support.scm
    (true? ,true?)
    (make-procedure ,make-procedure)
    (compound-procedure? ,compound-procedure?)
    (procedure-parameters ,procedure-parameters)
    (procedure-body ,procedure-body)
    (procedure-environment ,procedure-environment)
    (extend-environment ,extend-environment)
    (lookup-variable-value ,lookup-variable-value)
    (set-variable-value! ,set-variable-value!)
    (define-variable! ,define-variable!)
    (primitive-procedure? ,primitive-procedure?)
    (apply-primitive-procedure ,apply-primitive-procedure)
    (prompt-for-input ,prompt-for-input)
    (announce-output ,announce-output)
    (user-print ,user-print)
    (empty-arglist ,empty-arglist)
    (adjoin-arg ,adjoin-arg)
    (last-operand? ,last-operand?)
    (no-more-exps? ,no-more-exps?)     ;for non-tail-recursive machine
    (get-global-environment ,get-global-environment)

    (set-car! ,set-car!)
    (cons , cons)
    (car , car)
    (cdr , cdr)
    (list ,list)
    (__d ,__d)
    (exit ,exit)
    (make-thunk ,make-thunk)
    (thunk? ,thunk?)
    (thunk-env ,thunk-env)
    (thunk-exp ,thunk-exp)
    (evaluated-thunk? ,evaluated-thunk?)
    (thunk-value ,thunk-value)
    (thunk-set-value! ,thunk-set-value!)
    ))

(define eceval
        (make-machine
        '(exp env val proc argl continue unev)
        eceval-operations
        '(
;;;                                                                             REPL
;;; --------------------------------------------------------------------------------
read-eval-print-loop
        ;; the stack might not be empty after an error. reset it such
        ;; that stack statistics to refer only to stack operations
        ;; used to evaluate the previous input expression.
        (perform (op initialize-stack))
        (perform (op prompt-for-input) (const ";;; EC-Eval input:"))
        (assign exp (op read))
        (perform (op user-print) (reg exp))
        (assign env (op get-global-environment))
        (assign continue (label print-result))
        (goto (label actual-value))
print-result
        (perform (op print-stack-statistics/_/))  ; need monitored stack
        (perform (op announce-output) (const ";;; EC-Eval value:"))
        (perform (op user-print) (reg val))
        (goto (label read-eval-print-loop))

;;;                                                                        BAD INPUT
;;; --------------------------------------------------------------------------------
unknown-expression-type
        (assign val (const unknown-expression-type-error))
        (goto (label signal-error))

unknown-procedure-type
        (restore continue)
        (assign val (const unknown-procedure-type-error))
        (goto (label signal-error))

signal-error
        (perform (op user-print) (reg val))
        (goto (label read-eval-print-loop))

;;;                                                                             EVAL
;;; --------------------------------------------------------------------------------
eval-dispatch

        ;; for debugging
        ;; (perform (op print-stack-statistics/_/))

        (test (op self-evaluating?) (reg exp))
        (branch (label ev-self-eval))
        (test (op variable?) (reg exp))
        (branch (label ev-variable))
        (test (op quoted?) (reg exp))
        (branch (label ev-quoted))
        (test (op assignment?) (reg exp))
        (branch (label ev-assignment))
        (test (op definition?) (reg exp))
        (branch (label ev-definition))
        (test (op if?) (reg exp))
        (branch (label ev-if))
        (test (op lambda?) (reg exp))
        (branch (label ev-lambda))
        (test (op begin?) (reg exp))
        (branch (label ev-begin))
        (test (op application?) (reg exp))
        (branch (label ev-application))
        (goto (label unknown-expression-type))

;;;                                                          EVAL SIMPLE EXPRESSIONS
;;; --------------------------------------------------------------------------------
ev-self-eval
        (assign val (reg exp))
        (goto (reg continue))
ev-variable
        (assign val (op lookup-variable-value) (reg exp) (reg env))
        (goto (reg continue))
ev-quoted
        (assign val (op text-of-quotation) (reg exp))
        (goto (reg continue))
ev-lambda
        (assign unev (op lambda-parameters) (reg exp)) ; parameters
        (assign exp (op lambda-body) (reg exp))        ; body
        (assign val (op make-procedure) (reg unev) (reg exp) (reg env))
        (goto (reg continue))

;;;                                                             FUNCTION APPLICATION
;;; --------------------------------------------------------------------------------
ev-application
        ;; evaluate operator
        ;; unev contains the list of unevaluated operands.
        ;; argl accumulates the evaluated arguments
        ;; (perform (op __d) (const "\nAppl:") (reg exp))

        (save continue)
        (save env)
        (assign unev (op operands) (reg exp))
        (save unev)
        (assign exp (op operator) (reg exp))
        (assign continue (label ev-appl-did-operator))
        (goto (label actual-value))     ; ***
ev-appl-did-operator
        (restore unev)                  ; unevaluated operands
        (restore env)                   ; the same environment the operaror was evaluated in
        (assign argl (op empty-arglist))
        (assign proc (reg val))         ; evaluated operator
;;;                                                                   APPLY DISPATCH
;;; --------------------------------------------------------------------------------
        ;; ~ _continue_, is on the stack saved at ev-application
        (test (op primitive-procedure?) (reg proc))
        (branch (label primitive-apply))
        (test (op compound-procedure?) (reg proc))
        (branch (label compound-apply))
        (goto (label unknown-procedure-type))

;;;                                    PRIMITIVE APPLY -- NON TAIL-RECURSIVE-VERSION
;;; --------------------------------------------------------------------------------
primitive-apply
        (save proc)

arg-values-loop
        (test (op no-operands?) (reg unev))
        (branch (label primitive-apply0))
        (assign exp (op first-operand) (reg unev))
        (save unev)
        (save argl)
        (save env)
        (assign continue (label actual-argument-value))
        (goto (label actual-value))
actual-argument-value
        (restore env)
        (restore argl)
        (restore unev)
        (assign argl (op adjoin-arg) (reg val) (reg argl))
        (assign unev (op rest-operands) (reg unev))
        (goto (label arg-values-loop))

primitive-apply0
        (restore proc)
        (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
        (restore continue)
        (goto (reg continue))

;;;                                                                  COMPOUND APPLY
;;; --------------------------------------------------------------------------------
compound-apply
delay-args-loop
        (test (op no-operands?) (reg unev))
        (branch (label compound-apply0))
        (assign exp (op first-operand) (reg unev))
        (assign val (op make-thunk) (reg exp) (reg env))
        (assign argl (op adjoin-arg) (reg val) (reg argl))
        (assign unev (op rest-operands) (reg unev))
        (goto (label delay-args-loop))

compound-apply0
        (assign unev (op procedure-parameters) (reg proc))
        ;; the only place where the _env_ register is ever assigned a new value
        (assign env (op procedure-environment) (reg proc))
        (assign env (op extend-environment) (reg unev) (reg argl) (reg env))
        (assign unev (op procedure-body) (reg proc))
        (goto (label ev-sequence))

;;;                                 ACTUAL VALUE WITHOUT MEMOIZATION -- CALL-BY-NAME
;;; --------------------------------------------------------------------------------
;; actual-value                            ; same signature as eval-dispatch
;;         ;; receive: exp env continue
;;         ;; return: val
;;         (save continue)
;; actual-value-loop
;;         (assign continue (label force))
;;         (goto (label eval-dispatch))
;; force
;;         (test (op thunk?) (reg val))
;;         (branch (label force0))
;;         (restore continue)
;;         (goto (reg continue))
;; force0
;;         (assign exp (op thunk-exp) (reg val))
;;         (assign env (op thunk-env) (reg val))
;;         (goto (label actual-value-loop))

;;;                                    ACTUAL VALUE WITH MEMOIZATION -- CALL-BY-NEED
;;; --------------------------------------------------------------------------------
;;; logic implemented:
;;   (cond ((thunk? obj)
;;          (let ((result (actual-value (thunk-exp obj) (thunk-env obj))))
;;            (set-car! obj 'evaluated-thunk)
;;            (set-car! (cdr obj) result)  ; replace exp with its value
;;            (set-cdr! (cdr obj) '())     ; forget unneeded env
;;            result))
;;         ((evaluated-thunk? obj) (thunk-value obj))
;;         (else obj)))
        
actual-value                            ; same signature as eval-dispatch
        ;; receive: exp env continue
        ;; return: val
        ;; (perform (op __d) (const "\nexp:")  (reg exp))
        (save continue)
        (assign continue (label force))
        (goto (label eval-dispatch))
force
        (test (op thunk?) (reg val))
        (branch (label force0))
        (test (op evaluated-thunk?) (reg val))
        (branch (label return-evaluated-thunk))
        (goto (label return-actual-value))
force0
        (save val) ; save thunk as we need to mutate it after it has been evaluated
        (assign continue (label update-thunk-value))
        (assign exp (op thunk-exp) (reg val))
        (assign env (op thunk-env) (reg val))
        (goto (label actual-value))
update-thunk-value
        (restore exp)          ; contain the saved thunk to be mutated
        (perform (op thunk-set-value!) (reg exp) (reg val))
        (restore continue)
        (goto (reg continue))
return-evaluated-thunk
        (assign val (op thunk-value) (reg val))
return-actual-value
        (restore continue)
        ;;
        (goto (reg continue))

;;;                                                               SEQUENCE and BEGIN
;;; --------------------------------------------------------------------------------
ev-begin
        ;; ev-sequence supposes that it receives the _continue_ on the
        ;; stack and the list of unevaluated expressions in _unev_.
        (assign unev (op begin-actions) (reg exp))
        (save continue)
        (goto (label ev-sequence))
ev-sequence
        ;; _apply_ jumps here.  in that case the _continue_ register
        ;; is saved at ev-application.
        (assign exp (op first-exp) (reg unev))
        (test (op last-exp?) (reg unev))
        (branch (label ev-sequence-last-exp))
        (save unev)
        (save env)
        (assign continue (label ev-sequence-continue))
        (goto (label eval-dispatch))
ev-sequence-continue
        (restore env)
        (restore unev)
        (assign unev (op rest-exps) (reg unev))
        (goto (label ev-sequence))
ev-sequence-last-exp
;;;                                      LAST EXPRESSION IN SEQUENCE: TAIL RECURSION
;;; --------------------------------------------------------------------------------
        ;; The value of the whole sequence is the value of the last
        ;; expression.  The return address from _cotinue_ was saved by
        ;; ev-application or ev-begin
        ;; -- tail-recursive evaluator --
        ;; for this we need only a finite number of registers
        (restore continue)
        (goto (label eval-dispatch))
;;;         ;; -- non-tail-recursive evaluator --
;;;         (assign continue (label non-tail-recursive))
;;;         (goto (label eval-dispatch))
;;; non-tail-recursive
;;;         (restore continue)
;;;         (goto (reg continue))

;;;                                                                     CONDITIONALS
;;; --------------------------------------------------------------------------------
ev-if
        (save exp)
        (save env)
        (save continue)
        (assign continue (label ev-if-decide))
        (assign exp (op if-predicate) (reg exp))
        (goto (label actual-value))     ; ***
ev-if-decide
        (restore continue)
        (restore env)
        (restore exp)
        ;; (perform (op __d) (const "\nTest:") (reg val))
        (test (op true?) (reg val))
        (branch (label ev-if-consequent))
ev-if-alternative
        (assign exp (op if-alternative) (reg exp))
        (goto (label actual-value))     ; ***
ev-if-consequent
        (assign exp (op if-consequent) (reg exp))
        (goto (label actual-value))     ; ***

;;;                                                                     ASSSIGNMENT
;;; --------------------------------------------------------------------------------
ev-assignment
        (assign unev (op assignment-variable) (reg exp))
        (save unev)
        (assign exp (op assignment-value) (reg exp))
        (save env)
        (save continue)
        (assign continue (label ev-assignment-1))
        (goto (label eval-dispatch))
ev-assignment-1
        (restore continue)
        (restore env)
        (restore unev)
        (perform (op set-variable-value!) (reg unev) (reg val) (reg env))
        (assign val (const ok))
        (goto (reg continue))

;;;                                                                     DEFINITIONS
;;; --------------------------------------------------------------------------------
ev-definition
        (assign unev (op definition-variable) (reg exp))
        (save unev)
        (assign exp (op definition-value) (reg exp))
        (save env)
        (save continue)
        (assign continue (label ev-definition-1))
        (goto (label eval-dispatch))
ev-definition-1
        (restore continue)
        (restore env)
        (restore unev)
        (perform (op define-variable!) (reg unev) (reg val) (reg env))
        (assign val (const ok))
        (goto (reg continue))
        )))

(define the-global-environment (setup-environment))

(start eceval)

;;; ============================================================ TESTS

100

(+ 100 1)

(define fun (lambda (x) (+ 1 x)))

(fun 201)
;; (fun 0)
;; (fun -1)

(define (try a b)
  (if (= a 0) 'zero-argument b))

(try 0 (/ 1 0))
(try (fun -1) (/ 1 0))
(try (fun 0) 'b)

(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))

(define check
  (lambda (a b)
    (unless (= b 0)
      (/ a b)
      (begin (display "exception: returning -1")
             'division-by-zero))))

(check 0 1)
(check 1 0)

(define (append x y)
  (if (null? x)
      y
      (cons (car x)
            (append (cdr x) y))))

(append '(a b c) '(1 2 3))

;;; factorial -- tail-recursive
((lambda(s) (s s 20 (lambda(x) x)))
 (lambda(s n col)
   (if (= 0 n)
       (col 1)
       (s s (- n 1)
	  (lambda(r)
	    (col (* r n)))))))

;;; factorial -- tail-recursive
((lambda(s) (s s 20 1))
 (lambda(s n acc)
   (if (= 0 n)
       acc
       (s s (- n 1) (* acc n)))))

;;; factorial -- non tail-recursive
((lambda(s) (s s 20))
 (lambda(s n)
   (if (= 0 n)
       1
       (* (s s (- n 1)) n))))

(define (count n)
  (display " ")
  (display n)
  (if (= 0 n)
      0
      ((lambda (k)
         (display k)
         (display "\n")
         (+ 1 k) )
       (count (- n 1)))))

(define (count2 n)
  (display " . ")
  (display n)
  (if (= 0 n)
      0
      ((lambda (k)
         (display k)
         (display "\n")
         k)
       (+ 1 (count2 (- n 1))))))

(define (count3 n)
  (display " . ")
  (display n)
  (if (= 0 n)
      0
      ((lambda (k)
         (display "ok")
         'done)
       ;; this will never be evaluated !
       (+ 1 (count3 (- n 1))))))

;;; call by need will be different of call by name
(count 30)
(count2 30)
(count3 100)
