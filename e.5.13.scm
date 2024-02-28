
(load "aux/msim")

;; the assembler will lazy allocate registers during preprocessing

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
              ;; lazy allocation
              (begin (allocate-register name)
                     (lookup-register name)))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc insts))
                (execute)))))
      (lambda (message)
        (case message
          ('start                         ((set-contents! pc) the-instruction-sequence) (execute))
          ('install-instruction-sequence  (lambda (seq) (set! the-instruction-sequence seq)))

          ;; allocate-register disabled from outside
          ('allocate-register             (lambda _ _)) ; initially it called allocate-register

          ('get-register                  lookup-register)
          ('install-operations            (lambda (ops) (set! the-ops (append the-ops ops))))
          ('stack                         stack)
          ('operations                    the-ops)
          (else (error "Unknown request -- MACHINE" message)))))))

(define fib/iter
  (make-machine '(lazy-register-allocation)
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

(define fib/rec
  (make-machine
   '(lazy-register-allocation)
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
       (restore continue)                          
       (assign n (op -) (reg n) (const 2))
       (save continue)                             
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

(define sqrt/machine
  (make-machine
   '(lazy-register-allocation)
   `((< ,<) (- ,-) (* ,*) (abs ,abs) (+ ,+) (/ ,/))
   '(controller
     (assign guess (const 1.0))
     sqrt-iter
     
     (assign t1 (op *) (reg guess) (reg guess))
     (assign t2 (op -) (reg t1) (reg n))
     (assign t3 (op abs) (reg t2))
     (test (op <) (reg t3) (const 0.001))
     (branch (label done))
     
     (assign t1 (op /) (reg n) (reg guess))
     (assign t1 (op +) (reg t1) (reg guess))
     (assign guess (op /) (reg t1) (const 2))
     (goto (label sqrt-iter))
     done)))

(set-register-contents! sqrt/machine 'n 15)
(start sqrt/machine)
(__d (get-register-contents sqrt/machine 'guess))

(set-register-contents! fib/rec 'n 10)
(start fib/rec)
(__d (get-register-contents fib/rec 'val))

(set-register-contents! fib/iter 'n 10)
(start fib/iter)
(__d (get-register-contents fib/iter 'val))


