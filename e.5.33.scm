
(load "aux/sicp")
(load "aux/compiler")

;; use this: diff --side-by-side --color=always

(define test
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

(for-each test
          '(
            (define (factorial n)
               (if (= n 1)
                   1
                   (* (factorial (- n 1)) n)))
            
            (define (factorial n)
              (if (= n 1)
                  1
                  (* n (factorial (- n 1)))))

            (* n (factorial (- n 1)))
            (* (factorial (- n 1)) n)

            (* n ?)
            (* ? n)
            
            ))

;;; the execution times are identical in both cases.  It differs only
;;; the order in which the evaluated operands are accumulated in the
;;; argl register. see the diffences below, including the diffences of
;;; the subproblems.



;;;; The full difference
;;;; 
;;;; 
;;;;  expr............ (define (factorial n) (if (= n 1) 1 (* (factorial (- n 1)) n)))   |    expr............ (define (factorial n) (if (= n 1) 1 (* n (factorial (- n 1)))))
;;;;  reg needed...... (env)                                                                  reg needed...... (env)
;;;;  reg modified.... (val)                                                                  reg modified.... (val)
;;;; 
;;;;       (assign val (op make-compiled-procedure) (label entry2) (reg env))                      (assign val (op make-compiled-procedure) (label entry2) (reg env))
;;;;       (goto (label after-lambda1))                                                            (goto (label after-lambda1))
;;;;  entry2                                                                                  entry2
;;;;       (assign env (op compiled-procedure-env) (reg proc))                                     (assign env (op compiled-procedure-env) (reg proc))
;;;;       (assign env (op extend-environment) (const (n)) (reg argl) (reg env))                   (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
;;;;       (save continue)                                                                         (save continue)
;;;;       (save env)                                                                              (save env)
;;;;       (assign proc (op lookup-variable-value) (const =) (reg env))                            (assign proc (op lookup-variable-value) (const =) (reg env))
;;;;       (assign val (const 1))                                                                  (assign val (const 1))
;;;;       (assign argl (op list) (reg val))                                                       (assign argl (op list) (reg val))
;;;;       (assign val (op lookup-variable-value) (const n) (reg env))                             (assign val (op lookup-variable-value) (const n) (reg env))
;;;;       (assign argl (op cons) (reg val) (reg argl))                                            (assign argl (op cons) (reg val) (reg argl))
;;;;       (test (op primitive-procedure?) (reg proc))                                             (test (op primitive-procedure?) (reg proc))
;;;;       (branch (label primitive-branch17))                                                     (branch (label primitive-branch17))
;;;;  compiled-branch16                                                                       compiled-branch16
;;;;       (assign continue (label after-call15))                                                  (assign continue (label after-call15))
;;;;       (assign val (op compiled-procedure-entry) (reg proc))                                   (assign val (op compiled-procedure-entry) (reg proc))
;;;;       (goto (reg val))                                                                        (goto (reg val))
;;;;  primitive-branch17                                                                      primitive-branch17
;;;;       (assign val (op apply-primitive-procedure) (reg proc) (reg argl))                       (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;;;;  after-call15                                                                            after-call15
;;;;       (restore env)                                                                           (restore env)
;;;;       (restore continue)                                                                      (restore continue)
;;;;       (test (op false?) (reg val))                                                            (test (op false?) (reg val))
;;;;       (branch (label false-branch4))                                                          (branch (label false-branch4))
;;;;  true-branch5                                                                            true-branch5
;;;;       (assign val (const 1))                                                                  (assign val (const 1))
;;;;       (goto (reg continue))                                                                   (goto (reg continue))
;;;;  false-branch4                                                                           false-branch4
;;;;       (assign proc (op lookup-variable-value) (const *) (reg env))                            (assign proc (op lookup-variable-value) (const *) (reg env))
;;;;       (save continue)                                                                         (save continue)
;;;;       (save proc)                                                                             (save proc)
;;;;       (assign val (op lookup-variable-value) (const n) (reg env))                   |         (save env)
;;;;       (assign argl (op list) (reg val))                                             <
;;;;       (save argl)                                                                   <
;;;;       (assign proc (op lookup-variable-value) (const factorial) (reg env))                    (assign proc (op lookup-variable-value) (const factorial) (reg env))
;;;;       (save proc)                                                                             (save proc)
;;;;       (assign proc (op lookup-variable-value) (const -) (reg env))                            (assign proc (op lookup-variable-value) (const -) (reg env))
;;;;       (assign val (const 1))                                                                  (assign val (const 1))
;;;;       (assign argl (op list) (reg val))                                                       (assign argl (op list) (reg val))
;;;;       (assign val (op lookup-variable-value) (const n) (reg env))                             (assign val (op lookup-variable-value) (const n) (reg env))
;;;;       (assign argl (op cons) (reg val) (reg argl))                                            (assign argl (op cons) (reg val) (reg argl))
;;;;       (test (op primitive-procedure?) (reg proc))                                             (test (op primitive-procedure?) (reg proc))
;;;;       (branch (label primitive-branch8))                                                      (branch (label primitive-branch8))
;;;;  compiled-branch7                                                                        compiled-branch7
;;;;       (assign continue (label after-call6))                                                   (assign continue (label after-call6))
;;;;       (assign val (op compiled-procedure-entry) (reg proc))                                   (assign val (op compiled-procedure-entry) (reg proc))
;;;;       (goto (reg val))                                                                        (goto (reg val))
;;;;  primitive-branch8                                                                       primitive-branch8
;;;;       (assign val (op apply-primitive-procedure) (reg proc) (reg argl))                       (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;;;;  after-call6                                                                             after-call6
;;;;       (assign argl (op list) (reg val))                                                       (assign argl (op list) (reg val))
;;;;       (restore proc)                                                                          (restore proc)
;;;;       (test (op primitive-procedure?) (reg proc))                                             (test (op primitive-procedure?) (reg proc))
;;;;       (branch (label primitive-branch11))                                                     (branch (label primitive-branch11))
;;;;  compiled-branch10                                                                       compiled-branch10
;;;;       (assign continue (label after-call9))                                                   (assign continue (label after-call9))
;;;;       (assign val (op compiled-procedure-entry) (reg proc))                                   (assign val (op compiled-procedure-entry) (reg proc))
;;;;       (goto (reg val))                                                                        (goto (reg val))
;;;;  primitive-branch11                                                                      primitive-branch11
;;;;       (assign val (op apply-primitive-procedure) (reg proc) (reg argl))                       (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;;;;  after-call9                                                                             after-call9
;;;;       (restore argl)                                                                |         (assign argl (op list) (reg val))
;;;;                                                                                     >         (restore env)
;;;;                                                                                     >         (assign val (op lookup-variable-value) (const n) (reg env))
;;;;       (assign argl (op cons) (reg val) (reg argl))                                            (assign argl (op cons) (reg val) (reg argl))
;;;;       (restore proc)                                                                          (restore proc)
;;;;       (restore continue)                                                                      (restore continue)
;;;;       (test (op primitive-procedure?) (reg proc))                                             (test (op primitive-procedure?) (reg proc))
;;;;       (branch (label primitive-branch14))                                                     (branch (label primitive-branch14))
;;;;  compiled-branch13                                                                       compiled-branch13
;;;;       (assign val (op compiled-procedure-entry) (reg proc))                                   (assign val (op compiled-procedure-entry) (reg proc))
;;;;       (goto (reg val))                                                                        (goto (reg val))
;;;;  primitive-branch14                                                                      primitive-branch14
;;;;       (assign val (op apply-primitive-procedure) (reg proc) (reg argl))                       (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;;;;       (goto (reg continue))                                                                   (goto (reg continue))
;;;;  after-call12                                                                            after-call12
;;;;  after-if3                                                                               after-if3
;;;;  after-lambda1                                                                           after-lambda1
;;;;       (perform (op define-variable!) (const factorial) (reg val) (reg env))                   (perform (op define-variable!) (const factorial) (reg val) (reg env))
;;;;       (assign val (const ok))                                                                 (assign val (const ok))
;;;; 
;;;; 

;;; Subproblems compiled:


;;;;  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~                              ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~ 
;;;;  expr............ (* n (factorial (- n 1)))                                         |    expr............ (* (factorial (- n 1)) n)
;;;;  reg needed...... (env)                                                                  reg needed...... (env)
;;;;  reg modified.... (env proc argl continue val)                                           reg modified.... (env proc argl continue val)
;;;; 
;;;;       (assign proc (op lookup-variable-value) (const *) (reg env))                            (assign proc (op lookup-variable-value) (const *) (reg env))
;;;;       (save proc)                                                                             (save proc)
;;;;       (save env)                                                                    |         (assign val (op lookup-variable-value) (const n) (reg env))
;;;;                                                                                     >         (assign argl (op list) (reg val))
;;;;                                                                                     >         (save argl)
;;;;       (assign proc (op lookup-variable-value) (const factorial) (reg env))                    (assign proc (op lookup-variable-value) (const factorial) (reg env))
;;;;       (save proc)                                                                             (save proc)
;;;;       (assign proc (op lookup-variable-value) (const -) (reg env))                            (assign proc (op lookup-variable-value) (const -) (reg env))
;;;;       (assign val (const 1))                                                                  (assign val (const 1))
;;;;       (assign argl (op list) (reg val))                                                       (assign argl (op list) (reg val))
;;;;       (assign val (op lookup-variable-value) (const n) (reg env))                             (assign val (op lookup-variable-value) (const n) (reg env))
;;;;       (assign argl (op cons) (reg val) (reg argl))                                            (assign argl (op cons) (reg val) (reg argl))
;;;;       (test (op primitive-procedure?) (reg proc))                                             (test (op primitive-procedure?) (reg proc))
;;;;       (branch (label primitive-branch3))                                                      (branch (label primitive-branch3))
;;;;  compiled-branch2                                                                        compiled-branch2
;;;;       (assign continue (label after-call1))                                                   (assign continue (label after-call1))
;;;;       (assign val (op compiled-procedure-entry) (reg proc))                                   (assign val (op compiled-procedure-entry) (reg proc))
;;;;       (goto (reg val))                                                                        (goto (reg val))
;;;;  primitive-branch3                                                                       primitive-branch3
;;;;       (assign val (op apply-primitive-procedure) (reg proc) (reg argl))                       (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;;;;  after-call1                                                                             after-call1
;;;;       (assign argl (op list) (reg val))                                                       (assign argl (op list) (reg val))
;;;;       (restore proc)                                                                          (restore proc)
;;;;       (test (op primitive-procedure?) (reg proc))                                             (test (op primitive-procedure?) (reg proc))
;;;;       (branch (label primitive-branch6))                                                      (branch (label primitive-branch6))
;;;;  compiled-branch5                                                                        compiled-branch5
;;;;       (assign continue (label after-call4))                                                   (assign continue (label after-call4))
;;;;       (assign val (op compiled-procedure-entry) (reg proc))                                   (assign val (op compiled-procedure-entry) (reg proc))
;;;;       (goto (reg val))                                                                        (goto (reg val))
;;;;  primitive-branch6                                                                       primitive-branch6
;;;;       (assign val (op apply-primitive-procedure) (reg proc) (reg argl))                       (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;;;;  after-call4                                                                             after-call4
;;;;       (assign argl (op list) (reg val))                                             |         (restore argl)
;;;;       (restore env)                                                                 <
;;;;       (assign val (op lookup-variable-value) (const n) (reg env))                   <
;;;;       (assign argl (op cons) (reg val) (reg argl))                                            (assign argl (op cons) (reg val) (reg argl))
;;;;       (restore proc)                                                                          (restore proc)
;;;;       (test (op primitive-procedure?) (reg proc))                                             (test (op primitive-procedure?) (reg proc))
;;;;       (branch (label primitive-branch9))                                                      (branch (label primitive-branch9))
;;;;  compiled-branch8                                                                        compiled-branch8
;;;;       (assign continue (label after-call7))                                                   (assign continue (label after-call7))
;;;;       (assign val (op compiled-procedure-entry) (reg proc))                                   (assign val (op compiled-procedure-entry) (reg proc))
;;;;       (goto (reg val))                                                                        (goto (reg val))
;;;;  primitive-branch9                                                                       primitive-branch9
;;;;       (assign val (op apply-primitive-procedure) (reg proc) (reg argl))                       (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;;;;  after-call7                                                                             after-call7
;;;; 
;;;; 


;;; Even smaller subproblems:
;;;; 
;;;;  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~                       ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~
;;;;  expr............ (* n ?)                                                    |    expr............ (* ? n)
;;;;  reg needed...... (env)                                                           reg needed...... (env)
;;;;  reg modified.... (env proc argl continue val)                                    reg modified.... (env proc argl continue val)
;;;; 
;;;;       (assign proc (op lookup-variable-value) (const *) (reg env))                     (assign proc (op lookup-variable-value) (const *) (reg env))
;;;;       (assign val (op lookup-variable-value) (const ?) (reg env))            <
;;;;       (assign argl (op list) (reg val))                                      <
;;;;       (assign val (op lookup-variable-value) (const n) (reg env))                      (assign val (op lookup-variable-value) (const n) (reg env))
;;;;                                                                              >         (assign argl (op list) (reg val))
;;;;                                                                              >         (assign val (op lookup-variable-value) (const ?) (reg env))
;;;;       (assign argl (op cons) (reg val) (reg argl))                                     (assign argl (op cons) (reg val) (reg argl))
;;;;       (test (op primitive-procedure?) (reg proc))                                      (test (op primitive-procedure?) (reg proc))
;;;;       (branch (label primitive-branch3))                                               (branch (label primitive-branch3))
;;;;  compiled-branch2                                                                 compiled-branch2
;;;;       (assign continue (label after-call1))                                            (assign continue (label after-call1))
;;;;       (assign val (op compiled-procedure-entry) (reg proc))                            (assign val (op compiled-procedure-entry) (reg proc))
;;;;       (goto (reg val))                                                                 (goto (reg val))
;;;;  primitive-branch3                                                                primitive-branch3
;;;;       (assign val (op apply-primitive-procedure) (reg proc) (reg argl))                (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;;;;  after-call1                                                                      after-call1
;;;; 
;;;; 
;;;;

