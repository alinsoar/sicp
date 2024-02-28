
(load "aux/msim")

(define fib/iter
  (make-machine '(n acc1 val tmp)
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

;;; A000045    Fibonacci numbers: F(n) = F(n-1) + F(n-2) with F(0) = 0 and F(1) = 1.

(define fib/rec
  (make-machine
   '(n val continue)
   `((< ,<) (- ,-) (+ ,+))
   '(controller
     (assign continue (label ::fib-done))
     ;; LOOP will compute Fib(N) and return it to the provided CONTINUATION.

     ::fib-loop
       (test (op <) (reg n) (const 2))
       (branch (label ::immediate-answer))
       ;; set up to compute Fib(n-1)
       ;;
       ;; It pushes the provided continuation in the first frame and
       ;; after that it pushes ::afterfib-n-1 until the limit case is
       ;; reached.
       ;; 
       ;; The stack of ::fib-loop looks like this:
       ;; 
       ;;         INITIAL/CONTINUATION
       ;;         N
       ;;         ::afterfib-n-1
       ;;         N-1
       ;;         ::afterfib-n-1
       ;;         N-2
       ;;         ...
       ;;         ::afterfib-n-1
       ;;         2
       ;; 
       ;; and jumps to ::immediate-answer when N < 2
       ;; 
       (save continue)
       (assign continue (label ::afterfib-n-1))
       (save n)                                    ; save old value of n
       (assign n (op -) (reg n) (const 1))         ; clobber n to n-1
       (goto (label ::fib-loop))                   ; perform recursive call

     ::afterfib-n-1                                ; upon return here, val contains Fib(n-1)
       (restore n)
       (restore continue)                          ; this is unuseful
       ;; set up to compute Fib(n-2)
       (assign n (op -) (reg n) (const 2))
       (save continue)                             ; this is unuseful
       (assign continue (label ::afterfib-n-2))
       (save val)                                  ; save Fib(n-1)
       ;; ::fib-loop will compute Fib (n-2) and will
       ;; return the value to ::afterfib-n-2 in register `val`
       (goto (label ::fib-loop))
       
       ;; add the current VAL with the VAL stored on the stack.  In
       ;; ::AFTERFIB-N-1 is prepared ::LOOP to compute Fib(n-2) and
       ;; ::LOOP is instrumented to return here the value Fib(n-2).
     ::afterfib-n-2                                ; upon return here, val contains Fib(n-2)
       (assign n (reg val))                        ; n now contains Fib(n-2)
       (restore val)                               ; val now contains Fib(n-1)
       (restore continue)
       (assign val (op +) (reg val) (reg n))       ; Fib(n-1)+Fib(n-2)
       (goto (reg continue))                       ; return to caller, answer is in val

       ;; the register N will contain some limit case (N < 2)
     ::immediate-answer
       (assign val (reg n))                        ; base case: Fib(n)=n
       ;; the register CONTINUE will contain here either
       ;;   ::fib-done when initial N < 2
       ;;   ::afterfib-n-1 when ....
       ;;   ::afterfib-n-2 when ....
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
(test/interval fib/iter)
