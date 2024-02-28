
(define-test 'factorial-1
  (lambda ()

    (define factorial
      (make-pattern-operator
       (rule '(0) 1)
       (rule `((? n ,positive?))
             (* n (factorial (- n 1))))))

    (assert-equal (factorial 10)
                  3628800)))

(define-test 'factorial-2
  (lambda ()
    (define factorial (make-pattern-operator))

    (attach-rule! factorial (rule '(0) 1))

    (attach-rule! factorial
     (rule `((? n ,positive?))
           (* n (factorial (- n 1)))))

    (assert-equal (factorial 10)
                  3628800)))

(define-test 'quad
  (lambda ()

    (define quad
      (make-pattern-operator
       (rule
        `((? a) (? b) (? c) (? x))
        (+ (* a (expt x 2))
           (* b x)
           c))

       (rule
        `((? a) (? x) (? x) + (? b) (? x) + (? c))
        (+ (* a (expt x 2))
           (* b x)
           c))))

    (assert-equal (quad 1 2 3 4) 27)
    (assert-equal (quad 1 4 4 '+ 2 4 '+ 3) 27)))

(define-test 'frob
  (lambda ()

    (define frob
      (make-pattern-operator))

    (attach-rule! frob
     (rule
      '(a (?? x) (?? y) (?? x) c)
      (and (<= (length y) 2)
           y)))

    (assert-equal (apply frob '(a b b b b b b c))
                  '(b b))))