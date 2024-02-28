
(load "aux/msim")

(define test
  (lambda (machine n)
    (set-register-contents! fib/A 'n n)

    (start machine)

    (display (get-register-contents fib/A 'val))
    (newline)))

;;; behavior A -- current behavior

;;; the function is commented in 5.05 and the comments are removed here
(define fib/A
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
     ;; ---------- NO NEED TO SWEEP N AND VAL ----------
     ;; (assign n (reg val))
     ;; (restore val)
     (restore n)                        ; pop the stored VAL in register N
     (restore continue)
     (assign val (op +) (reg val) (reg n))
     (goto (reg continue))

     ::immediate-answer
     (assign val (reg n))
     (goto (reg continue))

     ::fib-done)))

((lambda (s) (s s 15))
 (lambda (s n)
   (or (zero? n)
       (begin
         (test fib/A n)
         (s s (- n 1))))))
