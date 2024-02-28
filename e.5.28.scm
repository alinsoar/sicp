

(load "aux/msim")

(set! make-stack make-stack2)

(load "aux/eceval")

;;; re-define eceval in non tail-recursive way
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
        (goto (label eval-dispatch))
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

;;;                                                                    ARG EVAL LOOP
;;; --------------------------------------------------------------------------------
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

;;;                                                                    EVAL LAST ARG
;;; --------------------------------------------------------------------------------
ev-appl-last-arg
        (assign continue (label ev-appl-accum-last-arg))
        (goto (label eval-dispatch))
ev-appl-accum-last-arg
        (restore argl)
        (assign argl (op adjoin-arg) (reg val) (reg argl))
        (restore proc)
        (goto (label apply-dispatch))   ; this is not really useful in this place

;;;                                                                           APPLY
;;; --------------------------------------------------------------------------------
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

;;;                                                               SEQUENCE and BEGIN
;;; --------------------------------------------------------------------------------
ev-begin
        ;; ev-sequence supposes that it receives the _continue_ on the
        ;; stack and the list of unevaluated expressions in _unev_.
        (assign unev (op begin-actions) (reg exp))
        (save continue)
        (goto (label ev-sequence))
;;; other (identical) non tail-recursive implementation is indide
;;; eveval by switching the code from sequence
ev-sequence
        (test (op no-more-exps?) (reg unev))
        (branch (label ev-sequence-end))
        (assign exp (op first-exp) (reg unev))
        (save unev)
        (save env)
        (assign continue (label ev-sequence-continue))
        (goto (label eval-dispatch))
ev-sequence-continue
        (restore env)
        (restore unev)
        (assign unev (op rest-exps) (reg unev))
        (goto (label ev-sequence))
ev-sequence-end
        (restore continue)
        (goto (reg continue))


;;;                                                                     CONDITIONALS
;;; --------------------------------------------------------------------------------
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


;;; k = a*n+b
;;; k1 = a*n1+b
;;; k2 = a*n2+b
;;; k2-k1 = a(n2-n1)
;;; a = (k2-k1)/(n2-n1)
;;; b = k1-a*n1 = k2-a*n2
(define compute-linear-coeff
  (lambda (n1 k1 n2 k2)
    ((lambda (a) ((lambda (b)
               (display "\na=") (display a)
               (display "\nb=") (display b)
               (display "\n"))
             (- k1 (* a n1))))
     (/ (- k2 k1) (- n2 n1)))))

;;; because we cannot get in the environment of eceval the statistics
;;; we precompute it here and insert the values in `stats`.
(compute-linear-coeff 1 18 50 1684)
(compute-linear-coeff 1 11 50 403)
(compute-linear-coeff 1 70 50 1883)
(compute-linear-coeff 1 17 50 164)

(define stats
  (lambda (n)
    ;; here are pasted the coefficients computed with compute-linear-coeff
    (define a1 34)
    (define b1 -16)
    (define a2 8)
    (define b2 3)
    (define a3 37)
    (define b3 33)
    (define a4 3)
    (define b4 14)
    (display "\nexpected pushes = ") (display (+ b1 (* a1 n)))
    (display "   ")                  (display (+ b2 (* a2 n)))
    (display "   ")                  (display (+ b3 (* a3 n)))
    (display "   ")                  (display (+ b4 (* a4 n)))
    (display "\n")))

(define (factorial/iter n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

(define (factorial/rec n)
  (if (= n 1)
      1
      (* (factorial/rec (- n 1)) n)))


;; maximum depth required to evaluate n! is 10 for each n.
(factorial/rec 1) (factorial/iter 1) (stats 1)
(factorial/rec 2) (factorial/iter 2) (stats 2)
(factorial/rec 3) (factorial/iter 3) (stats 3)
(factorial/rec 4) (factorial/iter 4) (stats 4)
(factorial/rec 5) (factorial/iter 5) (stats 5)
(factorial/rec 6) (factorial/iter 6) (stats 6)
(factorial/rec 7) (factorial/iter 7) (stats 7)
(factorial/rec 8) (factorial/iter 8) (stats 8)
(factorial/rec 9) (factorial/iter 9) (stats 9)
(factorial/rec 10) (factorial/iter 10) (stats 10)
(factorial/rec 11) (factorial/iter 11) (stats 11)
(factorial/rec 12) (factorial/iter 12) (stats 12)
(factorial/rec 13) (factorial/iter 13) (stats 13)
(factorial/rec 14) (factorial/iter 14) (stats 14)
(factorial/rec 15) (factorial/iter 15) (stats 15)
(factorial/rec 16) (factorial/iter 16) (stats 16)
(factorial/rec 17) (factorial/iter 17) (stats 17)
(factorial/rec 18) (factorial/iter 18) (stats 18)
(factorial/rec 19) (factorial/iter 19) (stats 19)
(factorial/rec 20) (factorial/iter 20) (stats 20)
(factorial/rec 21) (factorial/iter 21) (stats 21)
(factorial/rec 22) (factorial/iter 22) (stats 22)
(factorial/rec 23) (factorial/iter 23) (stats 23)
(factorial/rec 24) (factorial/iter 24) (stats 24)
(factorial/rec 25) (factorial/iter 25) (stats 25)
(factorial/rec 26) (factorial/iter 26) (stats 26)
(factorial/rec 27) (factorial/iter 27) (stats 27)
(factorial/rec 28) (factorial/iter 28) (stats 28)
(factorial/rec 29) (factorial/iter 29) (stats 29)
(factorial/rec 30) (factorial/iter 30) (stats 30)
(factorial/rec 31) (factorial/iter 31) (stats 31)
(factorial/rec 32) (factorial/iter 32) (stats 32)
(factorial/rec 33) (factorial/iter 33) (stats 33)
(factorial/rec 34) (factorial/iter 34) (stats 34)
(factorial/rec 35) (factorial/iter 35) (stats 35)
(factorial/rec 36) (factorial/iter 36) (stats 36)
(factorial/rec 37) (factorial/iter 37) (stats 37)
(factorial/rec 38) (factorial/iter 38) (stats 38)
(factorial/rec 39) (factorial/iter 39) (stats 39)
(factorial/rec 40) (factorial/iter 40) (stats 40)
(factorial/rec 41) (factorial/iter 41) (stats 41)
(factorial/rec 42) (factorial/iter 42) (stats 42)
(factorial/rec 43) (factorial/iter 43) (stats 43)
(factorial/rec 44) (factorial/iter 44) (stats 44)
(factorial/rec 45) (factorial/iter 45) (stats 45)
(factorial/rec 46) (factorial/iter 46) (stats 46)
(factorial/rec 47) (factorial/iter 47) (stats 47)
(factorial/rec 48) (factorial/iter 48) (stats 48)
(factorial/rec 49) (factorial/iter 49) (stats 49)
(factorial/rec 50) (factorial/iter 50) (stats 50)
(factorial/rec 51) (factorial/iter 51) (stats 51)
(factorial/rec 52) (factorial/iter 52) (stats 52)
(factorial/rec 53) (factorial/iter 53) (stats 53)
(factorial/rec 54) (factorial/iter 54) (stats 54)
(factorial/rec 55) (factorial/iter 55) (stats 55)
(factorial/rec 56) (factorial/iter 56) (stats 56)
(factorial/rec 57) (factorial/iter 57) (stats 57)
(factorial/rec 58) (factorial/iter 58) (stats 58)
(factorial/rec 59) (factorial/iter 59) (stats 59)
(factorial/rec 60) (factorial/iter 60) (stats 60)
(factorial/rec 61) (factorial/iter 61) (stats 61)
(factorial/rec 62) (factorial/iter 62) (stats 62)
(factorial/rec 63) (factorial/iter 63) (stats 63)
(factorial/rec 64) (factorial/iter 64) (stats 64)
(factorial/rec 65) (factorial/iter 65) (stats 65)
(factorial/rec 66) (factorial/iter 66) (stats 66)
(factorial/rec 67) (factorial/iter 67) (stats 67)
(factorial/rec 68) (factorial/iter 68) (stats 68)
(factorial/rec 69) (factorial/iter 69) (stats 69)
(factorial/rec 70) (factorial/iter 70) (stats 70)
(factorial/rec 71) (factorial/iter 71) (stats 71)
(factorial/rec 72) (factorial/iter 72) (stats 72)
(factorial/rec 73) (factorial/iter 73) (stats 73)
(factorial/rec 74) (factorial/iter 74) (stats 74)
(factorial/rec 75) (factorial/iter 75) (stats 75)
(factorial/rec 76) (factorial/iter 76) (stats 76)
(factorial/rec 77) (factorial/iter 77) (stats 77)
(factorial/rec 78) (factorial/iter 78) (stats 78)
(factorial/rec 79) (factorial/iter 79) (stats 79)
(factorial/rec 80) (factorial/iter 80) (stats 80)
(factorial/rec 81) (factorial/iter 81) (stats 81)
(factorial/rec 82) (factorial/iter 82) (stats 82)
(factorial/rec 83) (factorial/iter 83) (stats 83)
(factorial/rec 84) (factorial/iter 84) (stats 84)
(factorial/rec 85) (factorial/iter 85) (stats 85)
(factorial/rec 86) (factorial/iter 86) (stats 86)
(factorial/rec 87) (factorial/iter 87) (stats 87)
(factorial/rec 88) (factorial/iter 88) (stats 88)
(factorial/rec 89) (factorial/iter 89) (stats 89)
(factorial/rec 90) (factorial/iter 90) (stats 90)
(factorial/rec 91) (factorial/iter 91) (stats 91)
(factorial/rec 92) (factorial/iter 92) (stats 92)
(factorial/rec 93) (factorial/iter 93) (stats 93)
(factorial/rec 94) (factorial/iter 94) (stats 94)
(factorial/rec 95) (factorial/iter 95) (stats 95)
(factorial/rec 96) (factorial/iter 96) (stats 96)
(factorial/rec 97) (factorial/iter 97) (stats 97)
(factorial/rec 98) (factorial/iter 98) (stats 98)
(factorial/rec 99) (factorial/iter 99) (stats 99)
(factorial/rec 100) (factorial/iter 100) (stats 100)



