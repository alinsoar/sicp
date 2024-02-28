
(load "zdd")
(load "io")

(define test-add-zdd
  (lambda (sos set)
    (let ((a (add-set sos set)))
      (display "-->")
      (display (zdd-get-sets a))
      (newline)
      a)))

(define test-union
  (lambda (s1 s2)
    (let ((a (union s1 s2)))
      (display "=-=>")
      (display (zdd-get-sets a))
      (newline)
      a)))

(define test-print-set
  (lambda (sos)
    (display (zdd-get-sets sos))
    (newline)))

(define test-make-set
  (lambda (sos)
    ((lambda (s) (let ((set (s s sos)))
              (display "~~> ")
              (display (zdd-get-sets set))
              (newline)
              set))
     (lambda (s sos)
       (if (null? sos)
           zdd-empty-set
           (test-add-zdd (s s (cdr sos))
                         (car sos)))))))

(test-make-set
 '((1 2)
   (1 2 3)
   (1 7 8)
   ()
   (2 9 3)
   (1 2 7 8)))

(test-union
 (test-make-set
  '((1 2)
    ))
 (test-make-set
  '((1 2)
    (1 2 3) )))

