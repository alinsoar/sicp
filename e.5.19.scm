
(load "aux/msim")

;;; we keep the labels as pointers to code as in the original
;;; implementation.  we change the code format from
;;; (assign n (reg val)) . combination
;;; to
;;; (INDEX . (assign n (reg val))) . combination

(define (assemble controller-text machine)
  (extract-labels controller-text
                  (lambda (count insts labels)
                    ((machine 'set-code-size!) count)
                    ((machine 'initialize-breakpoint-table!) count)
                    ((machine 'initialize-labels!) labels)
                    (update-insts! insts labels machine)
                    insts)))

(define (extract-labels text receive)
  ((lambda (s)
     (newline)
     (__d "---")
     (s s text 0 0 receive))
   (lambda (s text n off receive)
     (if (null? text)
         (receive n '() '())
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
               (__d "\t" "\t" next-inst)
               (__d "\t" n "\t" off "\t" next-inst))
           (if (symbol? next-inst)
               (s s
                  (cdr text)
                  n
                  0
                  (lambda (count insts labels)
                    (receive
                        count
                        insts
                        (cons (make-label-entry next-inst insts) labels))))
               (s s
                  (cdr text)
                  (+ 1 n)
                  (+ off 1)
                  (lambda (count insts labels)
                    (receive
                        count
                        (cons (make-instruction (cons n next-inst) 'unset)
                              insts)
                        labels)))))))))

(define make-label-entry cons)
(define make-instruction cons)
(define label-index caaadr)

(define instruction-proc cdar)
(define instruction-text cdaar)
(define instruction-index caaar)

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    ;; (map __d labels)
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc!
        inst
        (make-execution-procedure
         (instruction-proc inst) labels machine pc flag stack ops)))
     insts)))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let ((the-ops `( (initialize-stack ,(lambda () (stack 'initialize)))
                      ;; only for stack2
                      (print-stack-statistics ,(lambda () (stack 'print-statistics)))))
          (register-table (list (list 'pc pc) (list 'flag flag)))
          ;; breakpoint interface
          (breakpoint-table 'not-initialized)
          (breakpoint-labels 'not-initialized)
          (code-size 'unset))
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

      ;; execution
      (define step
        (lambda (next break done)
          (let ((insts (get-contents pc)))
            (if (null? insts)
                (done)
                (if (vector-ref breakpoint-table (instruction-index insts))
                    (break insts)
                    (next insts))))))
      (define (execute)
        (step
         (lambda (insts)
           ((instruction-execution-proc insts))
           (execute))
         (breakpoint-repl execute)
         (lambda () 'done)))
      
      (define breakpoint-repl
        (lambda (continue)
          (lambda (insts)
            (define loop
              (lambda (insts)
                (let ((command (read)))
                  (__d ">" command)
                  (case (car command)
                    ('p (let ((r (lookup-register (cadr command))))
                          (__d "register" (cadr command) "=" (get-contents r))
                          (loop insts)))
                    ('s (__d "step") ((instruction-execution-proc insts))
                        (step
                         (lambda (insts)
                           (__d (instruction-index insts) "--" (instruction-text insts))
                           (loop insts))
                         (lambda (insts)
                           (__d (instruction-index insts) "--" (instruction-text insts))
                           (loop insts))
                         (lambda ()
                           'done)))
                    ('c (__d "continue") ((instruction-execution-proc insts))
                        (continue))
                    (else (error "unknown command" command))))))
            (__d "breakpoint at index" (instruction-index (get-contents pc)) "--" (instruction-text (get-contents pc)))
            (loop insts))))
      
      (define init-breakpoints!
        (lambda (n)
          ;; each entry with index X that is true means the execution
          ;; to stop at the instruction with index X.
          (set! breakpoint-table (make-vector n false))))
      (define init-labels!
        (lambda (labels)
          (set! breakpoint-labels labels)))
      
      (define check-breakpoint-index-validity
        (lambda (label offset succ fail)
          (let ((label (assoc label breakpoint-labels)))
            (let ((t (cdr label)))
              (if (not (null? t))
                  (let ((idx (+ offset (label-index label))))
                    (if (< idx code-size)
                        (succ idx)
                        (begin
                          (__d "invalid breakpoint index" idx)
                          (fail))))
                  (begin
                    (__d "invalid breakpoint index" idx " -- " label "+" offset)
                    (fail)))))))
      (define set-breakpoint!
        (lambda (label offset)
          (check-breakpoint-index-validity
           label offset
           (lambda (idx)
             (vector-set! breakpoint-table idx true)
             (__d "breakpoint set at index" idx))
           (lambda ()
             'fail))))
      (define cancel-breakpoint!
        (lambda (label offset)
          (check-breakpoint-index-validity
           label offset
           (lambda (idx)
             (if (vector-ref breakpoint-table idx)
                 (begin
                   (vector-set! breakpoint-table idx false)
                   (__d "breakpoint cancelled at index" idx))
                 (__d "breakpoint not set at index" idx)))
           (lambda ()
             'fail))))
      (define cancel-all-breakpoints!
        (lambda ()
          (vector-fill! breakpoint-table false)
          (__d "all breakpoints were cancelled")))
      
      (lambda (message)
        (case message
          ('start                         ((set-contents! pc) the-instruction-sequence) (execute))
          ('install-instruction-sequence  (lambda (seq) (set! the-instruction-sequence seq)))
          ('allocate-register             allocate-register)
          ('get-register                  lookup-register)
          ('install-operations            (lambda (ops) (set! the-ops (append the-ops ops))))
          ('stack                         stack)
          ('operations                    the-ops)
          ;; breakpoint interface
          ('initialize-breakpoint-table!  init-breakpoints!)
          ('initialize-labels!            init-labels!)
          ('set-code-size!                (lambda (c) (set! code-size c)))
          ('set-breakpoint!               set-breakpoint!)
          ('cancel-breakpoint!            cancel-breakpoint!)
          ('cancel-all-breakpoints!       (cancel-all-breakpoints!))
          (else (error "Unknown request -- MACHINE" message)))))))

(define set-breakpoint
  (lambda (machine label n)
    ((machine 'set-breakpoint!) label n)))
(define cancel-breakpoint
  (lambda (machine label n)
    ((machine 'cancel-breakpoint!) label n)))
(define cancel-all-breakpoints
  (lambda (machine)
    (machine 'cancel-all-breakpoints!)))
(define proceed-machine
  (lambda (machine)
    (machine 'continue)))

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

(define gcd
  (make-machine
   '(a b t)
   `((= ,=) (- ,-) (< ,<))
   '(controller
     test-b
     (test (op =) (reg b) (const 0))
     (branch (label gcd-done))
     (assign t (reg a))
     rem-loop
     (test (op <) (reg t) (reg b))
     (branch (label rem-done))
     (assign t (op -) (reg t) (reg b))
     (goto (label rem-loop))
     rem-done
     (assign a (reg b))
     (assign b (reg t))
     (goto (label test-b))
     gcd-done)))

(set-register-contents! fib/rec 'n 10)
(set-breakpoint fib/rec '::afterfib-n-1 2)
(cancel-all-breakpoints fib/rec)
(start fib/rec)
(__d (get-register-contents fib/rec 'val))

(set-register-contents! fib/iter 'n 10)
(set-breakpoint fib/iter '::loop 6)
(cancel-all-breakpoints fib/iter)
(start fib/iter)
(set-breakpoint fib/iter '::loop 6)
(set-breakpoint fib/iter '::loop 6)
(__d (get-register-contents fib/iter 'val))


(define read
  (let ((data '(
                (p a) (p b) (p t)
                (c)
                (s)
                (p a) (p b) (p t)
                (s)
                (p a) (p b) (p t)
                (c)
                (p a) (p b) (p t)
                )))
    (lambda ()
      (if (null? data)
          '(c)
          (let ((a (car data)))
            (set! data (cdr data))
            a)))))
(set-register-contents! gcd 'a 108)
(set-register-contents! gcd 'b 981)
(set-breakpoint gcd 'test-b 4)
(cancel-breakpoint gcd 'test-b 4)
(cancel-breakpoint gcd 'test-b 4)
(set-breakpoint gcd 'test-b 4)
; (cancel-all-breakpoints gcd)
(start gcd)
(__d "result:" (get-register-contents gcd 'a))
