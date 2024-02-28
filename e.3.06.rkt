#lang racket

;;For Section 3.1.2 -- written as suggested in footnote,
;; though the values of a, b, m may not be very "appropriately chosen"
(define (rand-update x)
  (let ((a 27) (b 26) (m 127))
    (modulo (+ (* a x) b) m)))

(define random-init 100)

(define rand0
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x) ) )

(define (iter n)
  (if (= n 0)
      '()
      (begin (display (rand))
             (display "\n")
             (iter (- n 1) ) ) ) )

(define rand 
  (let ((x random-init))
    (define (generate)
      (set! x (rand-update x) ) )
    (define (reset new)
      (set! x new)
      true)
    (define (dispatch msg)
      (cond ((eq? msg 'generate)
             (generate))
            ((eq? msg 'get)
             x)
            ((eq? msg 'reset)
             reset)
            (else "Unknown message") ) )
    dispatch) )

(module+ test
  (rand 'generate)
  (rand 'get)
  (rand 'generate)
  (rand 'get)
  (rand 'generate)
  (rand 'get)
  "---"
  ((rand 'reset) 100)
  (rand 'get)
  "---"
  (rand 'generate)
  (rand 'get)
  (rand 'generate)
  (rand 'get)
  (rand 'generate)
  (rand 'get)
  )

(module+ export
  (provide rand
           ;;rand-update
           ;;random-init
           ))
