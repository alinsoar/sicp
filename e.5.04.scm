
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

'(controller
  (assign continue (label done))
  loop
  (test (op =) (reg n) (const 0))
  (branch (label base-case))
  (save continue)
  (assign n (op -) (reg n) (const 1))
  (assign continue (label exp-end))
  (goto (label loop))
  exp-end
  (restore continue)
  (assign val (op *) (reg b) (reg val))
  (goto (reg continue))
  base-case
  (assign val (const 1))
  (goto (reg continue))
  done)

(define (expt b n)
  (define (expt-iter counter product)
    (if (= counter 0)
        product
        (expt-iter (- counter 1) (* b product))))
  (expt-iter n 1))

'(controller
  (assign counter (reg n))
  (assign product (const 1))
  loop
  (test (op =) (reg counter) (const 0))
  (branch (label done))
  (assign counter (op -) (reg counter) (const 1))
  (assign product (op *) (reg b) (reg product))
  (goto (label loop))
  done)

;;; tests are in the exercise 5.7 executed with the machine simulator
