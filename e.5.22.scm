
(load "aux/msim")

;; (define (append x y)
;;   (if (null? x)
;;       y
;;       (cons (car x) (append (cdr x) y))))
(define test/append
  (make-machine '(x y continue a)
                `((null? ,null?) (car ,car) (cdr ,cdr) (cons ,cons))
                '(controller
                  (assign continue (label done:))
                loop:
                  (test (op null?) (reg x))
                  (branch (label stop:))
                  (assign a (op car) (reg x))
                  (save a)
                  (save continue)
                  (assign x (op cdr) (reg x))
                  (assign continue (label next-a:))
                  (goto (label loop:))
                next-a:
                  (restore continue)
                  (restore a)
                  (assign y (op cons) (reg a) (reg y))
                  (goto (reg continue))
                stop:
                  (goto (reg continue))
                done:)))

;; (define (append! x y)
;;   (set-cdr! (last-pair x) y)
;;   x)
;; (define (last-pair x)
;;   (if (null? (cdr x))
;;       x
;;       (last-pair (cdr x))))
(define test/append!
  (make-machine '(x y tmp init-x)
                `((null? ,null?) (car ,car) (cdr ,cdr) (set-cdr! ,set-cdr!) )
                '(controller
                  (assign init-x (reg x))
                  (test (op null?) (reg x))
                  (branch (label null-x:))
                  (goto (label loop:))
                null-x:
                  (assign init-x (reg y))
                  (branch (label done:))
                loop:
                  (assign tmp (op cdr) (reg x))
                  (test (op null?) (reg tmp))
                  (branch (label last-pair:))
                  (assign x (reg tmp))
                  (goto (label loop:))
                last-pair:
                  (perform (op set-cdr!) (reg x) (reg y))
                done:
                  (assign x (reg init-x)))))

(define test
  (lambda (machine return/reg x y)
    (set-register-contents! machine 'x x)
    (set-register-contents! machine 'y y)
    (start machine)
    (__d (get-register-contents machine return/reg))))

(test test/append 'y '(1 2 3) '(4 5 6))
(test test/append 'y '() '(4 5 6))
(test test/append 'y '(1 2 3) '())
(test test/append 'y '() '())
(newline)
(test test/append! 'x '(1 2 3) '(4 5 6))
(test test/append! 'x '() '(4 5 6))
(test test/append! 'x '(1 2 3) '())
(test test/append! 'x '() '())



