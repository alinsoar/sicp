
(define (distinct l)
  (cond ((null? l) true)
        ((null? (cdr l)) true)
        ((member (car l) (cdr l)) false)
        (else (distinct (cdr l)))))

(define (require p)
  (if (not p) (amb) 'ok))

(define (place-value radix)
  (lambda (lst)
    (define (addup lst sum)
      (if (null? lst)
          sum
          (addup (cdr lst)
                 (+ (car lst)
                    (* radix sum)))))
    (addup lst 0)))

(define decimal (place-value 10))

(define (send-more-money)
  (let ((m 1)
        (s (amb 2 3 4 5 6 7 8 9)))
    (require (>= (+ s m 1) 10))
    (let ((d (amb 0 2 3 4 5 6 7 8 9))
          (e (amb 0 2 3 4 5 6 7 8 9))
          (y (amb 0 2 3 4 5 6 7 8 9)))
      (require (if (= y (+ d e))        ;OR
                   #t
                   (= y (+ d e -10))))
      (let ((n (amb 0 2 3 4 5 6 7 8 9))
            (o (amb 0 2 3 4 5 6 7 8 9))
            (r (amb 0 2 3 4 5 6 7 8 9)))
        (require (distinct (list m d e n o r s y)))
        (let ((send (decimal (list s e n d)))
              (more (decimal (list m o r e)))
              (money (decimal (list m o n e y))))
          (require (= money (+ send more)))
          (list send more money))))))

(begin
  (write-line (send-more-money))
  (amb))
'expect-write: '(9567 1085 10652)
'expect-value: 'no-more-alternatives

(define (donald-gerald-robert)
  (let ((d (amb 1 2 3 4 5 6 7 8 9))
        (g (amb 1 2 3 4 5 6 7 8 9))
        (r (amb 1 2 3 4 5 6 7 8 9))
        (o (amb 0 1 2 3 4 5 6 7 8 9))
        (n (amb 0 1 2 3 4 5 6 7 8 9))
        (a (amb 0 1 2 3 4 5 6 7 8 9))
        (l (amb 0 1 2 3 4 5 6 7 8 9))
        (e (amb 0 1 2 3 4 5 6 7 8 9))
        (b (amb 0 1 2 3 4 5 6 7 8 9))
        (t (amb 0 1 2 3 4 5 6 7 8 9)))
    (require (distinct (list d g r o n a l e b t)))
    (let ((donald (decimal (list d o n a l d)))
          (gerald (decimal (list g e r a l d)))
          (robert (decimal (list r o b e r t))))
      (require (= (+ donald gerald) robert))
      (list donald gerald robert))))
