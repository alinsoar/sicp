#lang racket

(require scheme/mpair)
(require compatibility/mlist)

(define (has-loop? l)
  "l MUST be a list."
  (define (loop x x2)
    "X2 moves at speed 2, and X with speed 1. Either X2 will meet X,
     or X2 will meet NIL."
    (display (format "~a ~a\n" x x2))
    (cond ((null? x2)
           #f)
          ((eq? x x2)
           #t)
          ((null? (mcdr x2))
           #f)
          (else
           (loop (mcdr x) (mcdr (mcdr x2))))))
  (loop l (mcdr l)))

(define (make-cycle x)
  (define (last-pair x)
    (if (null? (mcdr x))
        x
        (last-pair (mcdr x))))
  (set-mcdr! (last-pair x) x)
  x)

(module+ test
  (has-loop? (make-cycle (mlist 'a 'b 'c 'd)))
  (has-loop? (mlist 1 2 3 4)))


