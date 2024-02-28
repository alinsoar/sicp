
(load "aux/msim")

(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (let ((alloc-reg (machine 'allocate-register)))
      (for-each alloc-reg register-names))
    ((machine 'install-operations) ops)
    (machine 'reset-op!)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let ((the-ops `( (initialize-stack ,(lambda () (stack 'initialize)))
                      ;; only for stack2
                      (print-stack-statistics ,(lambda () (stack 'print-statistics)))))
          (register-table (list (list 'pc pc) (list 'flag flag)))
          (count-runtime-operations '_))
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
              (begin
                ((instruction-execution-proc insts))
                (set! count-runtime-operations (+ 1 count-runtime-operations))
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
          ('count-op                      count-runtime-operations)
          ('reset-op!                     (set! count-runtime-operations 0))
          (else (error "Unknown request -- MACHINE" message)))))))

(define fib/iter
  (make-machine '(acc1 sqrt/machine n val tmp)
                `((< ,<) (- ,-) (+ ,+))
                '(controller
                    (assign acc1 (const 1))
                    (assign val (const 1))
                  ::loop
                    (test (op <) (reg n) (const 3))
                    (branch (label ::done))
                    (assign n (op -) (reg n) (const 1))
                    (assign tmp (reg val))
                    (assign val (op +) (reg val) (reg acc1))
                    (assign acc1 (reg tmp))
                    (goto (label ::loop))
                  ::done)))


(set-register-contents! fib/iter 'n 10)
(start fib/iter)
(__d (get-register-contents fib/iter 'val))
(__d (fib/iter 'count-op))

(set-register-contents! fib/iter 'n 10)
(start fib/iter)
(__d (get-register-contents fib/iter 'val))
(__d (fib/iter 'count-op))

(__d (fib/iter 'reset-op!))


(set-register-contents! fib/iter 'n 10)
(start fib/iter)
(__d (get-register-contents fib/iter 'val))
(__d (fib/iter 'count-op))

