#lang racket

(require scheme/mpair)
(require compatibility/mlist)

;; checks for loops ONLY in LISTS
(define (has-loop? l)
  (let ((accumulator '()))
    (define (mem x list)
      (cond ((null? list)
             #f )
            ((eq? x (mcar list))
             #t )
            (else (mem x (mcdr list)))))
    (define (loop l)
      (cond ((or (null? l)
                 (not (mpair? l)))
             #f)
            ((mem l accumulator)
             #t)
            (else
             (set! accumulator (mcons l accumulator))
             (loop (mcdr l) ) ) ) )
    (if (loop l)
        "TRUE"
        "FALSE")))

;; example from 3.13
(define (make-cycle x)
  (define (last-pair x)
    (if (null? (mcdr x))
        x
        (last-pair (mcdr x))))
  (set-mcdr! (last-pair x) x)
  x)

(module+ test

  ;; LOOP forever
  (define a0* (mcons 'a '()))
  (define b0* (mcons 'b a0*))
  (set-mcdr! a0* b0*)
  (define c0* (mcons 'c b0*))

  ;; LOOP forever -- _very_subtle_ change: Instead of joining the CDR of
  ;; C with B, we join the CAR of C with B. It is false in this case, as
  ;; we are dealing with lists only.
  (define a1* (mcons 'a '()))
  (define b1* (mcons 'b a1*))
  (set-mcdr! a1* b1*)
  (define c1* (mcons b1* 'c))

  ;; SEVEN pairs
  (define a7 (mcons 'a 'a))
  (define b7 (mcons a7 a7))
  (define c7 (mcons b7 b7))

  ;; FOUR pairs
  (define a4 (mcons 'a 'a))
  (define b4 (mcons a4 a4))
  (define c4 (mcons b4 'c))

  "---"
  ;; LOOP forever -- the loop is on CAR
  (define a2* (mcons '() 'a))
  (define b2* (mcons a2* 'b))
  (set-mcar! a2* b2*)
  (define c2* (mcons 'c b2*))
  (define z2* (make-cycle (mlist 'a 'b 'c)))

  ;; No loop in simple lists
  (has-loop? (list->mlist '(a b c)))
  (has-loop? c0*)
  (has-loop? c1*)
  (has-loop? c7)
  (has-loop? c4)
  (has-loop? c2*)
  (has-loop? z2*)
  )
