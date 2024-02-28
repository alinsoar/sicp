
(load "aux/msim")
(load "aux/eceval-aux")
(load "aux/sicp")
(load "aux/compiler")

(set! make-stack make-stack2)

(define test/compile
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

(for-each test/compile
          '(
            ;; (define x 100)
            (* (+ x 2) 3)
            
            ))

;;; ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~

(define open-coded-primitive?
  (lambda (exp)
    (and (pair? exp)
         (memq (car exp) '(+ * =)))))

(define compile-open-coded-primitive
  (lambda (exp target linkage)
    (or (= (length exp) 3)
        (error "primitive operator" (operator exp) "requires 2 arguments"))
    (spread-arguments
     (operands exp)
     (lambda (arg1 arg2)
       (end-with-linkage linkage
                         (append-instruction-sequences
                          arg1
                          (preserving '(arg1)
                                      arg2
                                      (make-instruction-sequence
                                       '(arg1 arg2)
                                       (list target)
                                       `((assign ,target (op ,(operator exp)) (reg arg1) (reg arg2)))))))))))

;;; take an operand list and compile the given operands targeted to
;;; successive argument registers. An operand may contain a call to an
;;; open-coded primitive, so argument registers will have to be
;;; preserved during operand evaluation.
(define spread-arguments
  (lambda (rand-list return)
    (return (compile (first-operand rand-list) 'arg1 'next)
            (compile (first-operand (rest-operands rand-list)) 'arg2 'next))))

(define (compile exp target linkage)
  (cond ((self-evaluating? exp)       (compile-self-evaluating         exp  target linkage))
        ((quoted? exp)                (compile-quoted                  exp  target linkage))
        ((variable? exp)              (compile-variable                exp  target linkage))
        ((assignment? exp)            (compile-assignment              exp  target linkage))
        ((definition? exp)            (compile-definition              exp  target linkage))
        ((if? exp)                    (compile-if                      exp  target linkage))
        ((lambda? exp)                (compile-lambda                  exp  target linkage))
        ((begin? exp)                 (compile-sequence (begin-actions exp) target linkage))
        ((cond? exp)                  (compile          (cond->if      exp) target linkage))
        ((open-coded-primitive? exp)  (compile-open-coded-primitive    exp  target linkage)) 
        ((application? exp)           (compile-application             exp  target linkage))
        (else (error "Unknown expression type -- COMPILE" exp))))

(for-each test/compile
          '(
            (* (+ x 2) 3)
            
            (begin
              (define (factorial n)
                (define (iter product counter)
                  (if (> counter n)
                      product
                      (iter (* counter product)
                            (+ counter 1))))
                (iter 1 1))
              (factorial 25))

            ;; (begin
            ;;   (define (factorial n)
            ;;     (if (= n 1)
            ;;         1
            ;;         (* n (factorial (- n 1)))))
            ;;   (factorial 25))

            ))

;;; ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~

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
    (* ,*)
    (compiled-procedure-entry ,compiled-procedure-entry)
    (compiled-procedure-env ,compiled-procedure-env)
    (make-compiled-procedure ,make-compiled-procedure)
    (+ ,+)
    ))

(define code/0
  '(

    ;; a = 100
    (perform (op define-variable!) (const a) (const 100) (reg env))
    ;; val = a
    (assign val (op lookup-variable-value) (const a) (reg env))
    ;; val += 1
    (assign val (op +) (reg val) (const 1))


    ))

(define code/factorial/iter
  '(

      (assign val (op make-compiled-procedure) (label entry5) (reg env))
      (goto (label after-lambda4))
 entry5
      (assign env (op compiled-procedure-env) (reg proc))
      (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
      (assign val (op make-compiled-procedure) (label entry10) (reg env))
      (goto (label after-lambda9))
 entry10
      (assign env (op compiled-procedure-env) (reg proc))
      (assign env (op extend-environment) (const (product counter)) (reg argl) (reg env))
      (save continue)
      (save env)
      (assign proc (op lookup-variable-value) (const >) (reg env))
      (assign val (op lookup-variable-value) (const n) (reg env))
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
      (restore env)
      (restore continue)
      (test (op false?) (reg val))
      (branch (label false-branch12))
 true-branch13
      (assign val (op lookup-variable-value) (const product) (reg env))
      (goto (reg continue))
 false-branch12
      (assign proc (op lookup-variable-value) (const iter) (reg env))
      (assign arg1 (op lookup-variable-value) (const counter) (reg env))
      (assign arg2 (const 1))
      (assign val (op +) (reg arg1) (reg arg2))
      (assign argl (op list) (reg val))
      (assign arg1 (op lookup-variable-value) (const counter) (reg env))
      (assign arg2 (op lookup-variable-value) (const product) (reg env))
      (assign val (op *) (reg arg1) (reg arg2))
      (assign argl (op cons) (reg val) (reg argl))
      (test (op primitive-procedure?) (reg proc))
      (branch (label primitive-branch16))
 compiled-branch15
      (assign val (op compiled-procedure-entry) (reg proc))
      (goto (reg val))
 primitive-branch16
      (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
      (goto (reg continue))
 after-call14
 after-if11
 after-lambda9
      (perform (op define-variable!) (const iter) (reg val) (reg env))
      (assign val (const ok))
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
      (perform (op define-variable!) (const factorial) (reg val) (reg env))
      (assign val (const ok))
      (assign proc (op lookup-variable-value) (const factorial) (reg env))
      (assign val (const 20))
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

    ))

(define test/execution
  (lambda (code)

    ;; NOTE: we won't use the exp and unev registers in compiled code
    (define machine
      (make-machine
       '(env val proc argl continue unev arg1 arg2)
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

(test/execution code/0)
(test/execution code/factorial/iter)



