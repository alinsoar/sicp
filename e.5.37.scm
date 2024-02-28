
(load "aux/sicp")
(load "aux/compiler")

(define (no-preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
        (if ;; (and (needs-register? seq2 first-reg)
            ;;      (modifies-register? seq1 first-reg))
            true
            (no-preserving (cdr regs)
                           (make-instruction-sequence
                            (list-union (list first-reg)
                                        (registers-needed seq1))
                            (list-difference (registers-modified seq1)
                                             (list first-reg))
                            (append `((save ,first-reg))
                                    (statements seq1)
                                    `((restore ,first-reg))))
                           seq2)
            (no-preserving (cdr regs) seq1 seq2)))))
;; (set! preserving no-preserving)

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

            (+ 1 (+ 2 3) 4)
            
            ))

;;;  the output of compiling (+ 1 (+ 2 3) 4) looks like this in both
;;;  cases.  When preserving is active, only 2 saves occur.


;;;; 
;;;;  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~                      ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~  ~ 
;;;;  expr............ (+ 1 (+ 2 3) 4)                                                expr............ (+ 1 (+ 2 3) 4)
;;;;  reg needed...... (env)                                                       |  reg needed...... (env continue)
;;;;  reg modified.... (env proc argl continue val)                                   reg modified.... (env proc argl continue val)
;;;; 
;;;;                                                                               >       (save continue)
;;;;                                                                               >       (save env)
;;;;                                                                               >       (save continue)
;;;;       (assign proc (op lookup-variable-value) (const +) (reg env))                    (assign proc (op lookup-variable-value) (const +) (reg env))
;;;;                                                                               >       (restore continue)
;;;;                                                                               >       (restore env)
;;;;                                                                               >       (restore continue)
;;;;                                                                               >       (save continue)
;;;;       (save proc)                                                                     (save proc)
;;;;                                                                               >       (save env)
;;;;                                                                               >       (save continue)
;;;;       (assign val (const 4))                                                          (assign val (const 4))
;;;;                                                                               >       (restore continue)
;;;;       (assign argl (op list) (reg val))                                               (assign argl (op list) (reg val))
;;;;                                                                               >       (restore env)
;;;;                                                                               >       (save env)
;;;;       (save argl)                                                                     (save argl)
;;;;                                                                               >       (save continue)
;;;;                                                                               >       (save env)
;;;;                                                                               >       (save continue)
;;;;       (assign proc (op lookup-variable-value) (const +) (reg env))                    (assign proc (op lookup-variable-value) (const +) (reg env))
;;;;                                                                               >       (restore continue)
;;;;                                                                               >       (restore env)
;;;;                                                                               >       (restore continue)
;;;;                                                                               >       (save continue)
;;;;                                                                               >       (save proc)
;;;;                                                                               >       (save env)
;;;;                                                                               >       (save continue)
;;;;       (assign val (const 3))                                                          (assign val (const 3))
;;;;                                                                               >       (restore continue)
;;;;       (assign argl (op list) (reg val))                                               (assign argl (op list) (reg val))
;;;;                                                                               >       (restore env)
;;;;                                                                               >       (save argl)
;;;;                                                                               >       (save continue)
;;;;       (assign val (const 2))                                                          (assign val (const 2))
;;;;                                                                               >       (restore continue)
;;;;                                                                               >       (restore argl)
;;;;       (assign argl (op cons) (reg val) (reg argl))                                    (assign argl (op cons) (reg val) (reg argl))
;;;;                                                                               >       (restore proc)
;;;;                                                                               >       (restore continue)
;;;;       (test (op primitive-procedure?) (reg proc))                                     (test (op primitive-procedure?) (reg proc))
;;;;       (branch (label primitive-branch3))                                              (branch (label primitive-branch3))
;;;;  compiled-branch2                                                                compiled-branch2
;;;;       (assign continue (label after-call1))                                           (assign continue (label after-call1))
;;;;       (assign val (op compiled-procedure-entry) (reg proc))                           (assign val (op compiled-procedure-entry) (reg proc))
;;;;       (goto (reg val))                                                                (goto (reg val))
;;;;  primitive-branch3                                                               primitive-branch3
;;;;                                                                               >       (save continue)
;;;;       (assign val (op apply-primitive-procedure) (reg proc) (reg argl))               (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;;;;                                                                               >       (restore continue)
;;;;  after-call1                                                                     after-call1
;;;;       (restore argl)                                                                  (restore argl)
;;;;       (assign argl (op cons) (reg val) (reg argl))                                    (assign argl (op cons) (reg val) (reg argl))
;;;;                                                                               >       (restore env)
;;;;                                                                               >       (save argl)
;;;;                                                                               >       (save continue)
;;;;       (assign val (const 1))                                                          (assign val (const 1))
;;;;                                                                               >       (restore continue)
;;;;                                                                               >       (restore argl)
;;;;       (assign argl (op cons) (reg val) (reg argl))                                    (assign argl (op cons) (reg val) (reg argl))
;;;;       (restore proc)                                                                  (restore proc)
;;;;                                                                               >       (restore continue)
;;;;       (test (op primitive-procedure?) (reg proc))                                     (test (op primitive-procedure?) (reg proc))
;;;;       (branch (label primitive-branch6))                                              (branch (label primitive-branch6))
;;;;  compiled-branch5                                                                compiled-branch5
;;;;       (assign continue (label after-call4))                                           (assign continue (label after-call4))
;;;;       (assign val (op compiled-procedure-entry) (reg proc))                           (assign val (op compiled-procedure-entry) (reg proc))
;;;;       (goto (reg val))                                                                (goto (reg val))
;;;;  primitive-branch6                                                               primitive-branch6
;;;;                                                                               >       (save continue)
;;;;       (assign val (op apply-primitive-procedure) (reg proc) (reg argl))               (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;;;;                                                                               >       (restore continue)
;;;;  after-call4                                                                     after-call4
;;;; 
;;;; 
;;;;

