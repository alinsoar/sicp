
(load "aux/msim")

;; each time when the pushed register does not match the target
;; register a "." is printed.
(define :SAVE
  (lambda (stack reg pc++)
    (let ((reg/name (reg 'name)))
      (lambda ()
        (push stack (cons reg/name (get-contents reg)))
        (pc++)))))
(define :RESTORE
  (lambda (stack reg pc++)
    (let ((setter-reg (set-contents! reg))
          (name (reg 'name)))
      (lambda ()
        (let ((name-val (pop stack)))
          ;; to avoid cluterring the output with too many messages we
          ;; just display a point when the registers do not match.
          (or (eq? (car name-val) name) (display "."))
          (setter-reg (cdr name-val))    
          (pc++))))))

(define (make-register name)
  (let ((contents '*unassigned*))
    (lambda (message)
      (case message
        ('name name)
        ('get contents)
        ('set (lambda (value) (set! contents value)))
        (else (error "Unknown request -- REGISTER" message))))))

(define fib/rec1
  (make-machine '(n val continue)
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

(define fib/rec2
  (make-machine '(n val continue)
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
                    ;; (assign n (reg val))
                    ;; (restore val)
                    (restore n)
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
    ((lambda (s) (s s 15))
     (lambda (s n)
       (or (zero? n)
           (begin (test n test/fun 'val)
                  (s s (- n 1))))))))

(test/interval fib/rec1)
(test/interval fib/rec2)

