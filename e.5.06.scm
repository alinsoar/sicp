
(load "aux/msim")

;;; the comments are in 5.05 and are removed from here.
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
     ;; (restore continue)                             ; removed
     (assign n (op -) (reg n) (const 2))
     ;; (save continue)                                ; removed
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

(define test
  (lambda (n function result)
    (set-register-contents! function 'n n)
    (start function)
    (display (get-register-contents function result))
    (newline)))

(define test/interval
  (lambda (test/fun)
    ((lambda (s) (s s 20))
     (lambda (s n)
       (or (zero? n)
           (begin (test n test/fun 'val)
                  (s s (- n 1))))))))

(test/interval fib/rec)
