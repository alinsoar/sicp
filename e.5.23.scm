
;;; we can use a desugar procedure that is called only once in repl between read and eval.

;; or I put the desugaring in eceval, so it is desugared for each call.

;;; ~ add make-application in eceval-aux
;;; ~ add desugar
;;; ~ add in eceval-operations:
;;;    (desugar ,desugar)
;;;    (cond? ,cond?)
;;;    (cond->if ,cond->if)
;;; ~ add in execution the desugar for COND
;;;  ev-cond
;;;          (assign exp (op cond->if) (reg exp))
;;;          (goto (label ev-if))
;;; ~ it is never executed as COND is desugared. to execute it disable it from DESUGAR
;;; ((cond? exp)
;;;  (desugar (cond->if exp)))
;;; 
(load "aux/msim")
(load "aux/eceval-aux")

(set! make-stack make-stack2)

(define let? (tagged-list? 'let))
(define let-variables cadr)
(define let-variables-symbols (lambda (exp) (map car (let-variables exp))))
(define let-variables-values (lambda (exp) (map cadr (let-variables exp))))
(define let-body cddr)

(define desugar
  (lambda (exp)
    (cond ((self-evaluating? exp) exp)
          ((variable? exp) exp)
          ((lambda? exp)
           (let ((params (lambda-parameters exp))
                 (body (map desugar (lambda-body exp))))
             (make-lambda params body)))
          ((cond? exp)
           ;; this is also implemented in execution environment.  this
           ;; clause can be disabled here and desugared at runtime.
           (desugar (cond->if exp)))
          ((if? exp)
           (let ((pred (desugar (if-predicate exp)))
                 (consequent (desugar (if-consequent exp)))
                 (alternative (desugar (if-alternative exp))))
             (make-if pred consequent alternative)))
          ((let? exp)
           (let ((symbols (let-variables-symbols exp))
                 (values (map desugar (let-variables-values exp)))
                 (body (let-body exp)))
             (make-application (make-lambda symbols body)
                               values)))
          ((application? exp)
           (let ((rator (desugar (operator exp)))
                 (rands (map desugar (operands exp))))
             (make-application rator rands)))
          (else
           (__d "unknown expression --" exp)
           (exit 1)))))

(define code
'(
  
;;; factorial -- tail-recursive
((lambda(s) (s s 20 (lambda(x) x)))
 (lambda(s n col)
   (cond ((= 0 n) (col 1))
         (else
          (s s (- n 1)
	     (lambda(r)
	       (col (* r n))))))))

(define curry
  (lambda (x)
    (lambda (proc)
      (lambda (y)
        (proc x y)))))

(define 10+
  (let ((fun (let ((op +))
               ((curry 10) op))))
    fun))

(10+ 100)


;;; deactivating tail-recursion in ec-eval the stack will have the
;;; depth 109 for (count2 100) instead of 8, with tail recursion
;;; activated
(define (count2 n)
  (display " . ")
  (display n)
  (if (= 0 n)
      'ok
      ((lambda (k)
         (display " ")
         (display n))
       (count2 (- n 1)))))

(count2 20)


))

(define read
  (lambda ()
    "redefine the read function before to define eceval"
    (if (null? code)
        (exit 0)
        (let ((next (car code)))
          (set! code (cdr code))
          next))))

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

    (cons ,cons)

    (desugar ,desugar)
    (cond? ,cond?)
    (cond->if ,cond->if)
    ))

(define eceval
        (make-machine
        '(exp env val proc argl continue unev)
        eceval-operations
        (append
;;;                                                                             REPL
;;; --------------------------------------------------------------------------------
'(
read-eval-print-loop
        ;; the stack might not be empty after an error. reset it such
        ;; that stack statistics to refer only to stack operations
        ;; used to evaluate the previous input expression.
        (perform (op initialize-stack))
        (perform (op prompt-for-input) (const ";;; EC-Eval input:"))
        (assign exp (op read))
        (perform (op user-print) (reg exp))
        (assign exp (op desugar) (reg exp))
        (perform (op prompt-for-input) (const ";;; Desugared:"))
        (perform (op user-print) (reg exp))
        (assign env (op get-global-environment))
        (assign continue (label print-result))
        (goto (label eval-dispatch))
print-result
        (perform (op print-stack-statistics/_/))  ; need monitored stack
        (perform (op announce-output) (const ";;; EC-Eval value:"))
        (perform (op user-print) (reg val))
        (goto (label read-eval-print-loop))
)

;;;                                                                        BAD INPUT
;;; --------------------------------------------------------------------------------
'(
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
)

;;;                                                                             EVAL
;;; --------------------------------------------------------------------------------
'(
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

        (test (op cond?) (reg exp))
        (branch (label ev-cond))

        (test (op lambda?) (reg exp))
        (branch (label ev-lambda))
        (test (op begin?) (reg exp))
        (branch (label ev-begin))
        (test (op application?) (reg exp))
        (branch (label ev-application))
        (goto (label unknown-expression-type))
)

;;;                                                          EVAL SIMPLE EXPRESSIONS
;;; --------------------------------------------------------------------------------
'(
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
)

;;;                                                             FUNCTION APPLICATION
;;; --------------------------------------------------------------------------------
'(
ev-application
        ;; evaluate operator
        ;; unev contains the list of unevaluated operands.
        ;; argl accumulates the evaluated arguments
        (save continue)
        (save env)
        (assign unev (op operands) (reg exp))
        (save unev)
        (assign exp (op operator) (reg exp))
        (assign continue (label ev-appl-did-operator))
        (goto (label eval-dispatch))
ev-appl-did-operator
        ;; after having evaluated the operator prepare the loop for
        ;; evaluating each of the arguments
        (restore unev)                  ; unevaluated operands
        (restore env)                   ; the same environment the operaror was evaluated in
        (assign argl (op empty-arglist))
        (assign proc (reg val))         ; evaluated operator
        ;; limit case of a thunk
        (test (op no-operands?) (reg unev))
        (branch (label apply-dispatch))
        (save proc)
)

;;;                                              FUNCTION APPLICATION: ARG EVAL LOOP
;;; --------------------------------------------------------------------------------
'(
ev-appl-operand-loop
        ;; each step evaluates 1 operand from _unev_ and accumulate the result in _argl_.
        (save argl)
        (assign exp (op first-operand) (reg unev))
        ;; limit case of the last operand
        (test (op last-operand?) (reg unev))
        (branch (label ev-appl-last-arg))
        (save env)
        (save unev)
        (assign continue (label ev-appl-accumulate-arg))
        (goto (label eval-dispatch))
ev-appl-accumulate-arg
        (restore unev)
        (restore env)
        (restore argl)
        (assign argl (op adjoin-arg) (reg val) (reg argl))
        (assign unev (op rest-operands) (reg unev))
        (goto (label ev-appl-operand-loop))
)

;;;                                              FUNCTION APPLICATION: EVAL LAST ARG
;;; --------------------------------------------------------------------------------
'(
ev-appl-last-arg
        (assign continue (label ev-appl-accum-last-arg))
        (goto (label eval-dispatch))
ev-appl-accum-last-arg
        (restore argl)
        (assign argl (op adjoin-arg) (reg val) (reg argl))
        (restore proc)
        (goto (label apply-dispatch))   ; this is not really useful in this place
)

;;;                                                       FUNCTION APPLICATION:APPLY
;;; --------------------------------------------------------------------------------
'(
apply-dispatch
        ;; ~ _continue_, is on the stack, originally passed to
        ;;   eval-dispatch and saved at ev-application
        ;; ~ _proc_ contains the operator
        ;; ~ _argl_ contains the list of operands
        (test (op primitive-procedure?) (reg proc))
        (branch (label primitive-apply))
        (test (op compound-procedure?) (reg proc))
        (branch (label compound-apply))
        (goto (label unknown-procedure-type))
primitive-apply
        (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
        (restore continue)
        (goto (reg continue))
compound-apply
        (assign unev (op procedure-parameters) (reg proc))
        ;; the only place where the _env_ register is ever assigned a new value
        (assign env (op procedure-environment) (reg proc))
        (assign env (op extend-environment) (reg unev) (reg argl) (reg env))
        (assign unev (op procedure-body) (reg proc))
        (goto (label ev-sequence))
)

;;;                                                               SEQUENCE and BEGIN
;;; --------------------------------------------------------------------------------
'(
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
)

;;;                                      LAST EXPRESSION IN SEQUENCE: TAIL RECURSION
;;; --------------------------------------------------------------------------------
'(
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
)

;;;                                                                     CONDITIONALS
;;; --------------------------------------------------------------------------------

;;; +++ ADDED
'(
ev-cond
        (assign exp (op cond->if) (reg exp))
        (goto (label ev-if))
)

'(
ev-if
        (save exp)
        (save env)
        (save continue)
        (assign continue (label ev-if-decide))
        (assign exp (op if-predicate) (reg exp))
        (goto (label eval-dispatch))
ev-if-decide
        (restore continue)
        (restore env)
        (restore exp)
        (test (op true?) (reg val))
        (branch (label ev-if-consequent))
ev-if-alternative
        (assign exp (op if-alternative) (reg exp))
        (goto (label eval-dispatch))
ev-if-consequent
        (assign exp (op if-consequent) (reg exp))
        (goto (label eval-dispatch))
)

;;;                                                                     ASSSIGNMENT
;;; --------------------------------------------------------------------------------
'(
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
)

;;;                                                                     DEFINITIONS
;;; --------------------------------------------------------------------------------
'(
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
)

(define the-global-environment (setup-environment))

(start eceval)

