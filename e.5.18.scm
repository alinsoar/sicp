
(load "aux/msim")

(define (make-register name)
  (let ((contents '*unassigned*)
        (tracing false))
    (lambda (message)
      (case message
        ('get contents)
        ('set (lambda (value)
                (and tracing
                     (not (equal? value contents))
                     (__d "register" name "changed to" value))
                (set! contents value)))
        ('tracing-on! (set! tracing true))
        ('tracing-off! (set! tracing false))
        (else (error "Unknown request -- REGISTER" message))))))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()))
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
              (begin
                ((instruction-execution-proc insts))
                (execute)))))
      (define set-register-tracing
        (lambda (reg status)
          (let ((reg (assoc reg register-table)))
            (or reg (error "register" name "is not defined."))
            (case status
              ('on ((cadr reg) 'tracing-on!))
              ('off ((cadr reg) 'tracing-off!))
              (else (error "unknown tracing option" status))))))
      (lambda (message)
        (case message
          ('start                         ((set-contents! pc) the-instruction-sequence) (execute))
          ('install-instruction-sequence  (lambda (seq) (set! the-instruction-sequence seq)))
          ('allocate-register             allocate-register)
          ('get-register                  lookup-register)
          ('install-operations            (lambda (ops) (set! the-ops (append the-ops ops))))
          ('stack                         stack)
          ('operations                    the-ops)
          ('set-register-tracing!         set-register-tracing)
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
((fib/rec 'set-register-tracing!) 'val 'on)
(start fib/rec)
(__d (get-register-contents fib/rec 'val))




