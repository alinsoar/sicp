
(load "aux/msim")

;;; we start from the idea the number of stack operations is linear of
;;; the form a*N+b and try to compute A and B within the procedure TRY.

(define make-stack make-stack2)

(define fact
  (lambda ()
    (make-machine
     '(continue n val)
     `((= ,=) (- ,-) (* ,*))
     '(controller
       (assign continue (label fact-done))     ; set up final return address
       fact-loop
       (test (op =) (reg n) (const 1))
       (branch (label base-case))
       ;; Set up for the recursive call by saving n and continue.
       ;; Set up continue so that the computation will continue
       ;; at after-fact when the subroutine returns.
       (save continue)
       (save n)
       (assign n (op -) (reg n) (const 1))
       (assign continue (label after-fact))
       (goto (label fact-loop))
       after-fact
       (restore n)
       (restore continue)
       (assign val (op *) (reg n) (reg val))   ; val now contains n(n - 1)!
       (goto (reg continue))                   ; return to caller
       base-case
       (assign val (const 1))                  ; base case: 1! = 1
       (goto (reg continue))                   ; return to caller
       fact-done))))

(define try
  (lambda (machine n ret)
    (set-register-contents! machine 'n n)
    (start machine)
    (((machine 'stack) 'print-statistics) ret)))

;;; a*n1+b=c1
;;; a*n2+b=c2
;;; a*n1 - a*n2 = c1-c2
;;; a(n1-n2) = c1-c2
;;; a = (c1-c2)/(n1-n2)
;;; b = c1-a*n1 = c2-a*n2
(define compute-linear-coeff
  (lambda (k1 k2 return)
    (try (fact) k1
         (lambda (push1 total1)
           (try (fact) k2
                (lambda (push2 total2)
                  (let ((a (/ (- push2 push1) (- k2 k1))))
                    (let ((b (- push1 (* a k1))))
                      (return a b)))))))))

(define test
  (lambda (n machine)
    (set-register-contents! machine 'n n)
    (start machine)
    (__d "n =" n "val =" (get-register-contents machine 'val))
    (((cadr (assoc  'print-stack-statistics (machine 'operations)))) (lambda _ _))
    (newline) (newline)))

(compute-linear-coeff
 10 100
 (lambda (a b)
   ((lambda (s) (s s 30))
    (lambda (s n)
      (__d "expected:" (exact->inexact (+ (* a n) b)))
      (test n (fact))
      (if (= n 1) 'ok (s s (- n 1)))))))
