
;;; An unoptimal answer to an exercise

(define (curry-arguments . target-indices)
  (lambda (f)
    (let ((n (get-arity f))
          (m (length target-indices)))
      (define (the-combination . oargs)
        (assert (= (length oargs) (- n m)))
        (lambda nargs
          (assert (= (length nargs) m))
          (let lp ((is target-indices)
                   (args oargs)
                   (nargs nargs))
            (if (null? is)
                (apply f args)
                (lp (cdr is)
                    (list-insert args
                                 (car is)
                                 (car nargs))
                    (cdr nargs))))))
      (restrict-arity the-combination (- n m)))))

#|
((((curry-arguments 0 2)
   (lambda (x y z w)
     (list 'foo x y z w)))
  'a 'b)
 'c 'd)
;Value: (foo c a d b)
|#

(define (fan-out-argument . target-indices)
  (lambda (f)
    (let ((n (get-arity f))
          (m (length target-indices)))
      (define (the-combination . oargs)
        (assert (= (length oargs) (- n m)))
        (lambda (x)
          (let lp ((is target-indices)
                   (args oargs))
            (if (null? is)
                (apply f args)
                (lp (cdr is)
                    (list-insert args
                                 (car is)
                                 x))))))
      (restrict-arity the-combination (- n m)))))

#|
((((fan-out-argument 0 2)
   (lambda (x y z w)
     (list 'foo x y z w)))
  'a 'b)
 'c)
;Value: (foo c a c b)
|#
