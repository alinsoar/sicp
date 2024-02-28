#lang racket

(require (submod "e.2.67.rkt" export))
(require (submod "e.2.24.rkt" export))

(define (encode-symbol symbol tree)

  (define (assoc l co)
    (cond ((null? l)
           (error (format "symbol ~a not found" symbol)))
          ((eq? symbol (caar l))
           (co (cdar l)))
          (else (assoc (cdr l)
                       (lambda (x) (co x))))))
  (define (iter t path co)
    "first variant of creating the code associated to each symbol"
    (cond ((leaf? t)
           (co (list (cons (symbol-leaf t)
                           path))))
          (else (iter (left-branch t)
                      (append path '(0))
                      (lambda (x)
                        (iter (right-branch t)
                              (append path '(1))
                              (lambda (y)
                                (co (append x y)))))))))
  (define (iter2 t co)
    "2nd variant"
    (define (add sym set)
      (map (lambda (x) (cons (car x) (cons sym (cdr x))))
           set))
    (cond ((leaf? t)
           (co (list (list (symbol-leaf t)))))
          (else (iter2 (left-branch t)
                      (lambda (x)
                        (let ((x0 (add '0 x)))
                          (iter2 (right-branch t)
                                (lambda (y)
                                  (let ((y0 (add '1 y)))
                                    (co (append x0 y0)))))))))))
  (assoc (iter tree '() (lambda (x) x)) (lambda (x) x))
  (assoc (iter2 tree (lambda (x) x)) (lambda (x) x)))

(module+ test
  (encode-symbol 'B sample-tree)

  (tree-view sample-tree))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (test-encode-decode message)
  (equal? message
          (decode
           (encode message sample-tree)
           sample-tree)))

(module+ test
  (test-encode-decode '(A A A B B B C C C D D D))
  (test-encode-decode '(A))
  (test-encode-decode '(A B))
  (test-encode-decode '(A B X))
  (test-encode-decode '(A B C))
  (test-encode-decode '(A B C D)))

(module+ export
  (provide encode
           encode-symbol))

