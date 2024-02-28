
(load "aux/msim")

(set! DEBUG/EXECUTION true)


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


