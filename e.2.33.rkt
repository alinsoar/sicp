#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
              '()
              sequence))

(define (append seq1 seq2)
  (accumulate cons
              seq2
              seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y))
              0
              sequence))

(define length0
  (lambda (seq)
    "yet another implementation for length"
    (define replace-middle
      (lambda (l new)
        "replace the middle element or elements of SEQUENCE with NEW"
        (define (iter l n co)
          (if (null? l)
              (co '() 0)
              (iter (cdr l)
                    (+ 1 n)
                    (lambda (x m)
                      (co (cons (if (< (abs (- n m)) 2)
                                    new
                                    (car l))
                                x)
                          (+ m 1))))))
        (iter l 0 (lambda (x _) x))))
    (define count
      (lambda (seq)
        ;; at this point, the sequence will have in the middle either
        ;; '() or '() '() (if there are an odd (respectively even)
        ;; number of elements); the sequence has 2 or more
        ;; elements. We scan up to the middle.
        (define (iter l)
          (cond ((and (null? (car l)) (null? (cadr l))) 2)
                ((null? (car l)) 1)
                (else (+ 2 (iter (cdr l))))))
        (cond ((null? seq) 0)
              ((null? (cdr seq)) 1)
              (else (iter seq)))))
    (count (replace-middle seq '()))))

(module+ test

  (append '(1 2 3) '(4 5 6))

  (map (lambda (x) (* x x)) '(1 2 3))

  (length '(1 2 3))

  (map length0 '(()
                 (1)
                 (1 2)
                 (1 2 3)
                 (1 2 3 4))))

(module+ export
  (provide accumulate
           length))

