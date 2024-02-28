#lang racket

"                                       ;artist-mode
       3 ---------- x=1 -----> -3
       ^                        ^
       |                        |
       |                        |
       |                        |
      x=0                      x=0
       |                        |
       |                        |
       |                        |
       2 ------- x=1 ---------> 4
"

(define f
  ;; "f is an automaton with start state = 2"
  (let ((g 2))
    (lambda (x)
      (set! g
            (cond ((and (= g 2) (= x 0))  3)
                  ((and (= g 3) (= x 1)) -3)
                  ((and (= g 2) (= x 1)) 4)
                  ((and (= g 4) (= x 0)) -3)
                  (else 100)) )
      g) ) )

(define left-to-right
  (let ((left (f 0)))
    (let ((right (f 1)))
      (+ left right) ) ) )

(define right-to-left
  (let ((right (f 1)))
    (let ((left (f 0)))
      (+ left right) ) ) )

(module+ test
  "---"
  left-to-right
  right-to-left
  )
