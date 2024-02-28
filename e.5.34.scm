
(load "aux/msim")
(load "aux/eceval-aux")
(load "aux/sicp")
(load "aux/compiler")

(set! make-stack make-stack2)

(set! DEBUG/EXECUTION false)

(define generate-executable
  (lambda (expr)
    (set! label-counter 0)
    (for-each (lambda _ (display " ~ ")) (iota 20))
    (newline)
    (__d "expr............" expr)
    (let ((compiled-code (compile expr 'val 'next)))
      (__d "reg needed......" (car compiled-code))
      (__d "reg modified...." (cadr compiled-code))
      (newline)
      (map
        (lambda (x) (if (symbol? x)
                 (__d x)
                 (__d "    " x)))
        (caddr compiled-code)))))

(for-each generate-executable
          '(

            ;; (begin
            ;;  (define (factorial n)
            ;;    (define (iter product counter)
            ;;      (if (> counter n)
            ;;          product
            ;;          (iter (* counter product)
            ;;                (+ counter 1))))
            ;;    (iter 1 1))
            ;;  (factorial 25))

            ;; (begin
            ;;  (define (factorial n)
            ;;    (if (= n 1)
            ;;        1
            ;;        (* n (factorial (- n 1)))))
            ;;  (factorial 25))
            
            ))

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
    (no-more-exps? ,no-more-exps?)    ;for non-tail-recursive machine
    (get-global-environment ,get-global-environment)

    (cons , cons)
    (car ,car)
    ;; (car? ,car?)
    ;; (division? ,division?)
    (zero? ,zero?)
    (eq? ,eq?)
    (pair? ,pair?)
    (not ,not)
    ;; (primitive-symbol ,primitive-symbol)
    (__d ,__d)
    ;; (first-argument ,first-argument)
    ;; (second-argument ,second-argument)

    (false? ,false?)
    (list ,list)
    (compiled-procedure-entry ,compiled-procedure-entry)
    (compiled-procedure-env ,compiled-procedure-env)
    (make-compiled-procedure ,make-compiled-procedure)
    ))

(define code/rec
  '(
    ;; construct the procedure and skip over code for the procedure body
      (assign val (op make-compiled-procedure) (label entry5) (reg env))
      (goto (label after-lambda4))
 entry5 ; calls to factorial will enter here
      (assign env (op compiled-procedure-env) (reg proc))
      (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
      ;; begin actual procedure body of factorial
      (save continue)
      (save env)
      ;; compute (= n 1)
      (assign proc (op lookup-variable-value) (const =) (reg env))
      (assign val (const 1))
      (assign argl (op list) (reg val))
      (assign val (op lookup-variable-value) (const n) (reg env))
      (assign argl (op cons) (reg val) (reg argl))
      (test (op primitive-procedure?) (reg proc))
      (branch (label primitive-branch20))
 compiled-branch19
      ;; > is primitive, will enter here
      (assign continue (label after-call18))
      (assign val (op compiled-procedure-entry) (reg proc))
      (goto (reg val))
 primitive-branch20
      (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
 after-call18
      ;; val now contains result of (= n 1)
      (restore env)
      (restore continue)
      (test (op false?) (reg val))
      (branch (label false-branch7))
 true-branch8 ; limit case: (= 1 n) is true: return 1
      (assign val (const 1))
      (goto (reg continue))
 false-branch7
      ;; compute and return (* (factorial (- n 1)) n)
      (assign proc (op lookup-variable-value) (const *) (reg env))
               ;; THE RETURN ADDRESS OF CALLING (factorial (- n 1)) WILL ACCUMMULATE
               (save continue)
               ;; THIS *-OPERATOR WILL ACCUMULATE DURING THE RECURSIVE CALLS OF FACTORIAL
               (save proc) ; save * procedure
               (save env)
               ;; compute (factorial (- n 1))
               (assign proc (op lookup-variable-value) (const factorial) (reg env))
               (save proc) ; save factorial operator
                        ;; compute (- n 1)
                        (assign proc (op lookup-variable-value) (const -) (reg env))
                        (assign val (const 1))
                        (assign argl (op list) (reg val))
                        (assign val (op lookup-variable-value) (const n) (reg env))
                        (assign argl (op cons) (reg val) (reg argl))
                        (test (op primitive-procedure?) (reg proc))
                        (branch (label primitive-branch11))
                   compiled-branch10
                        (assign continue (label after-call9))
                        (assign val (op compiled-procedure-entry) (reg proc))
                        (goto (reg val))
                   primitive-branch11
                        (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
                   after-call9
                        ;; val now contains (- n 1)
                        (assign argl (op list) (reg val)) ; the operand is (- n 1)
               (restore proc) ; proc is factorial operator
               ;; apply factorial
               (test (op primitive-procedure?) (reg proc))
               (branch (label primitive-branch14))
          compiled-branch13 ; factorial call enters here, as it's combination
               (assign continue (label after-call12))
               (assign val (op compiled-procedure-entry) (reg proc))
               (goto (reg val))
          primitive-branch14
               (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
          after-call12
               ;; val now contains (factorial (- n 1))
               ;; The first (i.e. n) operand of * will be computed after the
               ;; factorial returns and will not be accummulated during the
               ;; recursive calls of *.
               (assign argl (op list) (reg val)) ; last argument of *
               (restore env)
               (assign val (op lookup-variable-value) (const n) (reg env))
               (assign argl (op cons) (reg val) (reg argl))
               (restore proc) ; proc is *
               (restore continue) ; return of factorial and of * (tail-recursive)
      ;; apply * and return its value
      (test (op primitive-procedure?) (reg proc))
      (branch (label primitive-branch17))
 compiled-branch16
      (assign val (op compiled-procedure-entry) (reg proc))
      (goto (reg val))
 primitive-branch17
      ;; factorial will return here tail-recusively
      (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
      (goto (reg continue))
 after-call15
 after-if6
 after-lambda4
      ;; assign the procedure to the variable `factorial` -- jump to `entry5`
      (perform (op define-variable!) (const factorial) (reg val) (reg env))
      (assign val (const ok))
      ;; call factorial(25)
      (assign proc (op lookup-variable-value) (const factorial) (reg env))
      (assign val (const 25))
      (assign argl (op list) (reg val))
      (test (op primitive-procedure?) (reg proc))
      (branch (label primitive-branch3))
 compiled-branch2
      (assign continue (label after-call1))
      (assign val (op compiled-procedure-entry) (reg proc))
      (goto (reg val))
 primitive-branch3
      (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
 after-call1
      ;; val now contains result of (= n 1)


 ))

(define code/iter
  '(
    ;; construct the procedure and skip over code for the procedure body
      (assign val (op make-compiled-procedure) (label entry5) (reg env))
      (goto (label after-lambda4))
 entry5 ; calls to factorial will enter here
      (assign env (op compiled-procedure-env) (reg proc))
      (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
      ;; construct the procedure `iter` and skip over code for the procedure body
      (assign val (op make-compiled-procedure) (label entry10) (reg env))
      (goto (label after-lambda9))
 entry10 ; calls to iter will enter here
      (assign env (op compiled-procedure-env) (reg proc))
      (assign env (op extend-environment) (const (product counter)) (reg argl) (reg env))
      ;; begin actual procedure body of iter      
      (save continue)
      (save env)
      ;; compute (> counter n)
      (assign proc (op lookup-variable-value) (const >) (reg env))
      (assign val (op lookup-variable-value) (const n) (reg env))
      (assign argl (op list) (reg val))
      (assign val (op lookup-variable-value) (const counter) (reg env))
      (assign argl (op cons) (reg val) (reg argl))
      (test (op primitive-procedure?) (reg proc))
      (branch (label primitive-branch25))
 compiled-branch24
      ;; > is primitive, will enter here
      (assign continue (label after-call23))
      (assign val (op compiled-procedure-entry) (reg proc))
      (goto (reg val))
 primitive-branch25
      (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
 after-call23
      ;; val now contains result of (> counter n)
      (restore env)
      (restore continue)
      (test (op false?) (reg val))
      (branch (label false-branch12))
 true-branch13 ; (> counter n) is true, return product
      (assign val (op lookup-variable-value) (const product) (reg env))
      (goto (reg continue))
 false-branch12
      ;; compute and return (iter (* counter product) (+ counter 1))
      (assign proc (op lookup-variable-value) (const iter) (reg env))
      (save continue)
      (save proc)
      (save env)
      (assign proc (op lookup-variable-value) (const +) (reg env))
      (assign val (const 1))
      (assign argl (op list) (reg val))
      (assign val (op lookup-variable-value) (const counter) (reg env))
      (assign argl (op cons) (reg val) (reg argl))
      (test (op primitive-procedure?) (reg proc))
      (branch (label primitive-branch19))
 compiled-branch18
      (assign continue (label after-call17))
      (assign val (op compiled-procedure-entry) (reg proc))
      (goto (reg val))
 primitive-branch19
      (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
 after-call17
      (assign argl (op list) (reg val))
      (restore env)
      (save argl)
      (assign proc (op lookup-variable-value) (const *) (reg env))
      (assign val (op lookup-variable-value) (const product) (reg env))
      (assign argl (op list) (reg val))
      (assign val (op lookup-variable-value) (const counter) (reg env))
      (assign argl (op cons) (reg val) (reg argl))
      (test (op primitive-procedure?) (reg proc))
      (branch (label primitive-branch16))
 compiled-branch15
      (assign continue (label after-call14))
      (assign val (op compiled-procedure-entry) (reg proc))
      (goto (reg val))
 primitive-branch16
      (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
 after-call14
      (restore argl)
      (assign argl (op cons) (reg val) (reg argl))
      (restore proc)
      (restore continue)
      (test (op primitive-procedure?) (reg proc))
      (branch (label primitive-branch22))
 compiled-branch21
      (assign val (op compiled-procedure-entry) (reg proc))
      (goto (reg val))
 primitive-branch22
      (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
      (goto (reg continue))
 after-call20
 after-if11
 after-lambda9
      ;; assign the procedure to the variable `iter` -- jump to `entry10`
      (perform (op define-variable!) (const iter) (reg val) (reg env))
      (assign val (const ok))
      ;; call iter(1, 1) with the linkage `return`
      (assign proc (op lookup-variable-value) (const iter) (reg env))
      (assign val (const 1))
      (assign argl (op list) (reg val))
      (assign val (const 1))
      (assign argl (op cons) (reg val) (reg argl))
      (test (op primitive-procedure?) (reg proc))
      (branch (label primitive-branch8))
 compiled-branch7
      (assign val (op compiled-procedure-entry) (reg proc))
      (goto (reg val))
 primitive-branch8
      (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
      (goto (reg continue))
 after-call6
 after-lambda4
      ;; assign the procedure to the variable `factorial` -- jump to `entry5`
      (perform (op define-variable!) (const factorial) (reg val) (reg env))
      (assign val (const ok))
      ;; call factorial(25)
      (assign proc (op lookup-variable-value) (const factorial) (reg env))
      (assign val (const 25))
      (assign argl (op list) (reg val))
      (test (op primitive-procedure?) (reg proc))
      (branch (label primitive-branch3))
 compiled-branch2
      (assign continue (label after-call1))
      (assign val (op compiled-procedure-entry) (reg proc))
      (goto (reg val))
 primitive-branch3
      (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
 after-call1
      ;; val now contains result of (= n 1)


 ))

(define test
  (lambda (code)

    ;; NOTE: we won't use the exp and unev registers in compiled code
    (define machine
      (make-machine
       '(env val proc argl continue unev n)
       eceval-operations
       code))
    (set-register-contents! machine 'env (setup-environment))
    (start machine)
    (((machine 'stack) 'get-statistics)
     (lambda (p1 t1)
       (__d "result:" (get-register-contents machine 'val))
       (__d "total-pushes" p1 "maximum-depth" t1)
       (__d "~ ~ ~")
       (newline)))))

;;; results for factorial(25)

 ;; result: 15511210043330985984000000
 ;; total-pushes 146 maximum-depth 74
 ;; ~ ~ ~

 ;; result: 15511210043330985984000000
 ;; total-pushes 152 maximum-depth 3
 ;; ~ ~ ~

;;; the value of *-operator, the environment and the return address
;;; will accumulate during the recursive calls of factorial in (* n
;;; (factorial (- n 1))), as marked above.  The `n` operand will not
;;; be accummulated because the operands are computed in reversed
;;; order.

;;; In the iterative case all calls are done tail-recursively, so
;;; there is no recursive call between a PUSH and the corresponding
;;; POP.

(test code/rec)
(test code/iter)


