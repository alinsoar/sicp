
(load "aux/msim")

;;; we change only the syntax of the label -- from a symbol L to (label L).

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
                      (lambda (insts labels)
                        (let ((next-inst (car text)))
                          (if (eq? 'controller next-inst)
                              (receive insts labels)
                              (if (label? next-inst)
                                  (receive insts (cons (make-label-entry (get-label next-inst) insts) labels))
                                  (receive (cons (make-instruction next-inst) insts) labels))))))))
(define label? (tagged-list? 'label))
(define get-label cadr)

(define exp-iter-machine
  (make-machine '(n counter b product)
                `((= ,=) (- ,-) (* ,*))
                '(controller
                  (assign counter (reg n))
                  (assign product (const 1))
                  (label loop)          ; changed label syntax
                  (test (op =) (reg counter) (const 0))
                  (branch (label done))
                  (assign counter (op -) (reg counter) (const 1))
                  (assign product (op *) (reg b) (reg product))
                  (goto (label loop))
                  (label done)          ; changed label syntax
                  )))

(set-register-contents! exp-iter-machine 'n 3)
(set-register-contents! exp-iter-machine 'b 2)
(start exp-iter-machine)
(display (get-register-contents exp-iter-machine 'product))
(newline)


