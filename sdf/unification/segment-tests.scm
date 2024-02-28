
(define (unify-test p1 p2 expected-dict)
  (let ((actual-dict (unify p1 p2)))
    (if (not (equal-dicts? actual-dict expected-dict))
        (begin
          (fresh-line)
          (write-string "failure:")
          (newline)
          (pp `((p1 ,p1)
                (p2 ,p2)
                (actual-dict ,actual-dict)
                (expected-dict ,expected-dict)))))))

(define (equal-dicts? d1 d2)
  (or (eqv? d1 d2)
      (and d1
           d2
           (= (length d1) (length d2))
           (every (lambda (b1 b2)
                    (and (eq? (car b1) (car b2))
                         (equal? (cadr b1) (cadr b2))))
                  d1
                  d2))))

(unify-test '(a ((? b) 2 3) 1 c)
            '(a (1 2 3) 1 c)
            '((b 1)))

(unify-test `(a ((? b ,number?) 2 3) 1 c)
            '(a (1 2 3) 1 c)
            '((b 1)))

(unify-test `(a ((? b ,symbol?) 2 3) 1 c)
            '(a (1 2 3) 1 c)
            #f)

(unify-test '(a ((? b) 2 3) (? b) c)
            '(a (1 2 3) 2 c)
            #f)

(unify-test '(a ((? b) 2 3) (? b) c)
            '(a (1 2 3) 1 c)
            '((b 1)))

(unify-test '(a ((? b) 2 3) (? b) c)
            '(a (1 2 3) 1 (? x))
            '((x c) (b 1)))

(unify-test '(a ((? b) 2 3) (? b) c)
            '(a (1 2 3) (? x) c)
            '((x 1) (b 1)))

(unify-test '(a (?? x) c)
            '(a b b b b b b c)
            '((x (b b b b b b))))

(unify-test '(a (?? x) (?? x) c)
            '(a 1 2 3 1 2 3 c)
            '((x (1 2 3))))

(unify-test '(a (?? x) (?? y) (?? x) c)
            '(a 1 2 3 1 2 3 c)
            '((y ()) (x (1 2 3))))

(unify-test '(a (?? y) (?? x) (?? x) c)
            '(a 1 2 3 1 2 3 c)
            '((x ()) (y (1 2 3 1 2 3))))

(unify-test '(a (?? x) (?? x) c)
            '(a b b b b b b c)
            '((x (b b b))))

(unify-test '(a (?? x) (?? y) (?? x) c)
            '(a b b b b b b c)
            '((y ()) (x (b b b))))

(define (segment-test t1 t2)
  (let ((results '()))
    (unify:internal t1 t2 '()
      (lambda (dict)
        ;;(pp dict)
        (set! results
              (cons (list (match:equivalent-patterns? t1 t2 dict)
                          t1 t2 dict)
                    results))
        #f))
    (if (every car results)
        #t
        (pp (filter (lambda (x) (not (car x)))
                    results)))))

(segment-test '(a (?? x) (?? y) (?? x) c) '(a b b b b b b c))
;Value: #t

(segment-test '(a (?? x) (?? y) (?? x) c) '(a b b (?? z) b b b b c))
;Value: #t

(segment-test '(a (?? x) (?? y) (?? x) c)
              '(a b b (?? z) b b (?? w) b b (?? z) c))
;Value: #t

(segment-test '(a (?? x) (?? y) (?? x) c (?? u))
              '(a b b (?? z) b b (?? w) b b (?? z) c (?? v)))
;Value: #t

(segment-test '(a (?? x) (?? y) (?? x) c (?? u))
              '((? m) b b (?? z) b b (?? w) b b (?? z) c (?? v)))
;Value: #t
