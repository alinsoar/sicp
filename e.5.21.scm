
(load "aux/msim")

;;; recursive
(define count-leaves/rec
  (make-machine '(tree count continue)
                `((not ,not)
                  (null? ,null?) (pair? ,pair?)
                  (car ,car) (cdr ,cdr)
                  (+ ,+))
                '(controller
                  (assign count (const 0))
                  (assign continue (label done:))
                loop:
                  (test (op null?) (reg tree))
                  (branch (label null:))
                  (goto (label not-null:))
                null:
                  (goto (reg continue))
                not-null:
                  (test (op pair?) (reg tree))
                  (assign flag (op not) (reg flag))
                  (branch (label not-pair:))
                  ;; pair case
                  (save continue)
                  (save tree)
                  (assign tree (op car) (reg tree))
                  (assign continue (label pair-cdr:))
                  (goto (label loop:))
                pair-cdr:
                  (restore tree)
                  (restore continue)
                  (assign tree (op cdr) (reg tree))
                  (goto (label loop:))
                not-pair:
                  (assign count (op +) (const 1) (reg count))
                  (goto (reg continue))
                done:)))

;;; recursive, keep count on stack
(define count-leaves/iter
  (make-machine '(tree count continue tmp)
                `((not ,not)
                  (null? ,null?) (pair? ,pair?)
                  (car ,car) (cdr ,cdr)
                  (+ ,+))
                '(controller
                  (assign continue (label done:))
                loop:
                  (test (op null?) (reg tree))
                  (branch (label null:))
                  (goto (label not-null:))
                null:
                  (assign count (const 0))
                  (goto (reg continue))
                not-null:
                  (test (op pair?) (reg tree))
                  (assign flag (op not) (reg flag))
                  (branch (label not-pair:))
                  ;; pair
                  (save continue)
                  (save tree)
                  (assign tree (op car) (reg tree))
                  (assign continue (label pair-car:))
                  (goto (label loop:))
                pair-car:
                  (restore tree)
                  (save count)
                  (assign tree (op cdr) (reg tree))
                  (assign continue (label pair-cdr:))
                  (goto (label loop:))
                pair-cdr:
                  (restore tmp)         ; tmp will keep the leaves-count of car
                  (restore continue)    ; continue was saved at not-null:pair
                  (assign count (op +) (reg count) (reg tmp))
                  (goto (reg continue))
                pair-acc:
                not-pair:
                  (assign count (const 1))
                  (goto (reg continue))
                done:)))

(define test
  (lambda (machine tree)
    (set-register-contents! machine 'tree tree)
    (start machine)
    (__d (get-register-contents machine 'count))))

(test count-leaves/rec '(1 (2 2) (3 (4 5 (6)))))
(test count-leaves/rec '(10 20 ()))
(test count-leaves/rec '(()))
(newline)
(test count-leaves/iter '(1 (2 2) (3 (4 5 (6)))))
(test count-leaves/iter '(10 20 ()))
(test count-leaves/iter '(()))
