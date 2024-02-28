
(load "aux/msim")

;; (define (expt b n)
;;   (if (= n 0)
;;       1
;;       (* b (expt b (- n 1)))))

(define exp-rec-machine
  (make-machine
   '(n val b continue)
   `((= ,=) (- ,-) (* ,*)
     (print ,(lambda(a) (display ":") (display a) (newline))))
   '(controller
     (assign continue (label done))
     loop
     ;; if (= n 0)
     (test (op =) (reg n) (const 0))
     (branch (label base-case))
     (save continue)
     (assign continue (label exp-end))
     (perform (op print) (reg n))
     (assign n (op -) (reg n) (const 1))
     (goto (label loop))
     exp-end
     ;; b=/=0 => return b*(b^(n-1))
     (restore continue)
     ;; register B does not change. this multiplication
     ;; is executed N times as there are stacked N
     ;; continuations to exp-end.
     (assign val (op *) (reg b) (reg val))
     (goto (reg continue))
     base-case
     ;; n=0 => return 1
     (assign val (const 1))
     (goto (reg continue))
     done)))

;; (define (expt b n)
;;   (define (expt-iter counter product)
;;     (if (= counter 0)
;;         product
;;         (expt-iter (- counter 1) (* b product))))
;;   (expt-iter n 1))

(define exp-iter-machine
  (make-machine
   '(n counter b product)
   `((= ,=) (- ,-) (* ,*))
   '(controller
     (assign counter (reg n))
     (assign product (const 1))
     loop
     (test (op =) (reg counter) (const 0))
     (branch (label done))
     (assign counter (op -) (reg counter) (const 1))
     (assign product (op *) (reg b) (reg product))
     (goto (label loop))
     done)))

(define test
  (lambda (e n machine result)
    (set-register-contents! machine 'n n)
    (set-register-contents! machine 'b e)
    (start machine)
    (display (get-register-contents machine result))
    (newline)))

(test 2 3 exp-iter-machine 'product)
(test 2 10 exp-iter-machine 'product)
(test 2 3 exp-rec-machine 'val)
(test 2 10 exp-rec-machine 'val)
