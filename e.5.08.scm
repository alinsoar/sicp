
(load "aux/msim")

(define machine1
  (lambda ()
    (make-machine
     '(a)
     '()
     '(start
       (goto (label here))
       here
       (assign a (const 3))
       (goto (label there))
       here
       (assign a (const 4))
       (goto (label there))
       there))))

(define machine2
  (lambda ()
    (make-machine
     '(a)
     `((print ,(lambda(a) (display ":") (display a) (newline))))
     '(start
       (goto (label here2))
       here
       (assign a (const 3))
       here2
       (assign a (const 4))
       (perform (op print) (reg a))
       (goto (label there))
       there))))

(define test
  (lambda (machine)
    (set-register-contents! machine 'a -2)
    (start machine)
    (display (get-register-contents machine 'a))
    (newline)))

(test (machine1))
(test (machine2))
(newline)

;;; redefine extract-labels
(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels
       (cdr text)
       (lambda (insts labels)
         (let ((next-inst (car text)))
           (cond ((and (symbol? next-inst)
                       (assoc next-inst labels))
                  (display "duplicated label -- ")
                  (display next-inst)
                  (newline)
                  (receive insts
                      (cons (make-label-entry next-inst insts)
                            labels)))
                 ((symbol? next-inst)
                  (receive insts
                      (cons (make-label-entry next-inst insts)
                            labels)))
                 (else
                  (receive (cons (make-instruction next-inst)
                                 insts)
                      labels))))))))

;;; redefine the machines using the new definition of labels
(define machine1
  (lambda ()
    (make-machine
     '(a)
     '()
     '(start
       (goto (label here))
       here
       (assign a (const 3))
       (goto (label there))
       here
       (assign a (const 4))
       (goto (label there))
       there))))

(define machine2
  (lambda ()
    (make-machine
     '(a)
     '()
     '(start
       (goto (label here2))
       here
       (assign a (const 3))
       here2
       (assign a (const 4))
       (goto (label there))
       there))))

(test (machine1))
(test (machine2))
