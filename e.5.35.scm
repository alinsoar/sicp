
(define f
  (lambda (x)
    (+ x (g (+ x 2)))))



;;;      (assign val (op make-compiled-procedure) (label entry16) (reg env))
;;;      (goto (label after-lambda15))
;;;    entry16                                 ; f
;;;      (assign env (op compiled-procedure-env) (reg proc))
;;;      ;; f has a parameter x
;;;      (assign env (op extend-environment) (const (x)) (reg argl) (reg env))
;;;      ;; body of f
;;;      (assign proc (op lookup-variable-value) (const +) (reg env))
;;;      (save continue)
;;;      (save proc) ; save `+`
;;;      (save env)
;;;              (assign proc (op lookup-variable-value) (const g) (reg env))
;;;              (save proc) ; save `g`
;;;              (assign proc (op lookup-variable-value) (const +) (reg env))
;;;              (assign val (const 2))
;;;              (assign argl (op list) (reg val))
;;;              (assign val (op lookup-variable-value) (const x) (reg env))
;;;              (assign argl (op cons) (reg val) (reg argl))
;;;              (test (op primitive-procedure?) (reg proc))
;;;              (branch (label primitive-branch19))
;;;            compiled-branch18
;;;              (assign continue (label after-call17))
;;;              (assign val (op compiled-procedure-entry) (reg proc))
;;;              (goto (reg val))
;;;            primitive-branch19
;;;              (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;;;            ;; val contains (+ 2 x)
;;;            after-call17
;;;              (assign argl (op list) (reg val))
;;;              (restore proc) ; proc is `g`
;;;      ;; apply `g`
;;;      (test (op primitive-procedure?) (reg proc))
;;;      (branch (label primitive-branch22))
;;;    compiled-branch21
;;;      (assign continue (label after-call20))
;;;      (assign val (op compiled-procedure-entry) (reg proc))
;;;      (goto (reg val))
;;;    primitive-branch22
;;;      (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;;;      ;; at this point `val` contains g(x+2)
;;;    after-call20
;;;      (assign argl (op list) (reg val))
;;;      (restore env)
;;;      (assign val (op lookup-variable-value) (const x) (reg env))
;;;      (assign argl (op cons) (reg val) (reg argl))
;;;      (restore proc) ; restore `+`
;;;      (restore continue)
;;;      (test (op primitive-procedure?) (reg proc))
;;;      (branch (label primitive-branch25)) ; + is primitive
;;;    compiled-branch24
;;;      (assign val (op compiled-procedure-entry) (reg proc))
;;;      (goto (reg val))
;;;    primitive-branch25
;;;      ;; will return here (+ x (g (+ x 2)))
;;;      (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;;;      (goto (reg continue))
;;;    after-call23
;;;    after-lambda15 ; end of `f` body
;;;      (perform (op define-variable!) (const f) (reg val) (reg env))
;;;      (assign val (const ok))



