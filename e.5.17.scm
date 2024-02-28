
(load "aux/msim")

;;; to simplify, we suppose each instruction may have at most 1 label

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations))
        (inv/labels
         (map (lambda (l)
                ;; if there are multiple labels to the same
                ;; instruction, only one of them is considered.
                (cons (if (pair? (cdr l))
                          (cadr l)
                          ;; this is a label at the end of code
                          '())
                      (car l)))
              labels)))
    ;; ((machine 'install-labels!) labels)
    (for-each
     (lambda (inst)
       (let ((label (assoc inst inv/labels)))
         (set-instruction-execution-proc!
          inst
          (cons (and label (cdr label))
                (make-execution-procedure
                 (instruction-text inst) labels machine pc flag stack ops)))))
     insts)))

(define (instruction-execution-proc inst)
  (cdar inst))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (labels '()))
    (let ((the-ops `( (initialize-stack ,(lambda () (stack 'initialize)))
                      ;; only for stack2
                      (print-stack-statistics ,(lambda () (stack 'print-statistics)))))
          (register-table (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register:" name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (let ((label+instr (instruction-execution-proc insts)))
                (and (car label+instr)
                     (__d "label" (car label+instr)))
                (__d (caar insts))
                ((cdr label+instr))
                (execute)))))
      (lambda (message)
        (case message
          ('start                         ((set-contents! pc) the-instruction-sequence) (execute))
          ('install-instruction-sequence  (lambda (seq) (set! the-instruction-sequence seq)))
          ('allocate-register             allocate-register)
          ('get-register                  lookup-register)
          ('install-operations            (lambda (ops) (set! the-ops (append the-ops ops))))
          ('stack                         stack)
          ('operations                    the-ops)
          (else (error "Unknown request -- MACHINE" message)))))))

(define fib/rec
  (make-machine
   '(n val continue)
   `((< ,<) (- ,-) (+ ,+))
   '(controller
     (assign continue (label ::fib-done))
     ::fib-loop
     (test (op <) (reg n) (const 2))
     (branch (label ::immediate-answer))
     (save continue)
     (assign continue (label ::afterfib-n-1))
     (save n)
     (assign n (op -) (reg n) (const 1))
     (goto (label ::fib-loop))
     ::afterfib-n-1
     (restore n)
     (assign n (op -) (reg n) (const 2))
     (assign continue (label ::afterfib-n-2))
     (save val)
     (goto (label ::fib-loop))
     ::afterfib-n-2
     (assign n (reg val))
     (restore val)
     (restore continue)
     (assign val (op +) (reg val) (reg n))
     (goto (reg continue))
     ::immediate-answer
     (assign val (reg n))
     (goto (reg continue))
     ::fib-done)))

(set-register-contents! fib/rec 'n 7)
(start fib/rec)
(__d (get-register-contents fib/rec 'val))




