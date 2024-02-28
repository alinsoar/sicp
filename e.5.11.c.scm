
(load "aux/msim")

;;; we keep the machine parameter STACK but we do not use it any more.

;;; each assembler pop and push instruction will act on the stack
;;; attached to the register parameter.

(define (make-register name)
  (let ((contents '*unassigned*)
        (stack (make-stack)))
    (stack 'initialize)
    (lambda (message)
      (case message
        ('stack stack)
        ('get contents)
        ('set (lambda (value) (set! contents value)))
        (else (error "Unknown request -- REGISTER" message))))))

(define :SAVE
  (lambda (_ reg pc++)
    (let ((stack/reg (reg 'stack)))
      (lambda ()
        (push stack/reg (get-contents reg))
        (pc++)))))
(define :RESTORE
  (lambda (_ reg pc++)
    (let ((setter-reg (set-contents! reg))
          (stack/reg (reg 'stack)))
      (lambda ()
        ;; reset the machine stack after each operation
        ((fib 'stack) 'initialize)
        
        (setter-reg (pop stack/reg))    
        (pc++)))))

(define fib
  (make-machine
   '(continue n val)
   `((< ,<) (- ,-) (+ ,+))
   '(controller
     (assign continue (label fib-done))
     fib-loop
     (test (op <) (reg n) (const 2))
     (branch (label immediate-answer))
     (save continue)
     (assign continue (label afterfib-n-1))
     (save n)
     (assign n (op -) (reg n) (const 1))
     (goto (label fib-loop))
     afterfib-n-1
     (restore n)
     (assign n (op -) (reg n) (const 2))
     (assign continue (label afterfib-n-2))
     (save val)
     (goto (label fib-loop))
     afterfib-n-2
     (assign n (reg val))
     (restore val)
     (restore continue)
     (assign val (op +) (reg val) (reg n))
     (goto (reg continue))
     immediate-answer
     (assign val (reg n))
     (goto (reg continue))
     fib-done)))

(define fib2
  (make-machine
   '(continue n val)
   `((< ,<) (- ,-) (+ ,+))
   '(controller
     (assign continue (label fib-done))
     fib-loop
     (test (op <) (reg n) (const 2))
     (branch (label immediate-answer))
     (save n)                   ; we permute saving N and CONTINUE
     (save continue)
     (assign continue (label afterfib-n-1))
     (assign n (op -) (reg n) (const 1))
     (goto (label fib-loop))
     afterfib-n-1
     (restore n)
     (assign n (op -) (reg n) (const 2))
     (assign continue (label afterfib-n-2))
     (save val)
     (goto (label fib-loop))
     afterfib-n-2
     (assign n (reg val))
     (restore continue)      ; we permute popping VAL and CONTINUE
     (restore val)
     (assign val (op +) (reg val) (reg n))
     (goto (reg continue))
     immediate-answer
     (assign val (reg n))
     (goto (reg continue))
     fib-done)))

(define test
  (make-machine
   '(n m)
   `((= ,=) (display ,__d))
   '( (save n)
      (save m)
      (assign n (const 40))
      (assign m (const 10))
      (save n)
      (save m)

      (restore n) (perform (op display) (reg n))
      (restore n) (perform (op display) (reg n))
      (restore m) (perform (op display) (reg m))
      (restore m) (perform (op display) (reg m)))))

(set-register-contents! fib 'n 15)
(start fib)
(__d (get-register-contents fib 'val))

(set-register-contents! fib2 'n 15)
(start fib2)
(__d (get-register-contents fib2 'val))

(set-register-contents! test 'n 8)
(set-register-contents! test 'm 33)
(start test)
