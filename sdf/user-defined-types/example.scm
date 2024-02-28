
(load "tagging/code/load")
*** output flushed ***
;Value: tagging

(define (slow-prime? n)
  (and (n:exact-positive-integer? n)
       (n:>= n 2)
       (let loop ((k 2))
         (or (n:> (n:square k) n)
             (and (not (n:= (n:remainder n k) 0))
                  (loop (n:+ k 1)))))))

(define prime-number?
  (simple-abstract-predicate 'prime-number slow-prime?))

(define make-prime-number
  (predicate-constructor prime-number?))

(define short-list-of-primes
  (list (make-prime-number 2)
        (make-prime-number 7)
        (make-prime-number 31)))

(make-prime-number 4)
;Ill-formed data for this constructor: 4
;To continue, call RESTART with an option number:
; (RESTART 1) => Return to read-eval-print level 1.
;Start debugger? (y or n):

(number? (make-prime-number 2))
;Value: #f

(set-predicate<=! prime-number? number?)

(number? (make-prime-number 2))
;Value: #t

(predicate? number?)
;Value: #t

(define numeric-vector?
  (make-vector-predicate number?))
;Value: numeric-vector?

(vector-predicate-element numeric-vector?)
;Value 76: #[compiled-procedure 76 ("arith" #x9d) #x14 #x10555dd64]

(define numeric-vector (vector-constructor number?))
;Value: numeric-vector

(numeric-vector 1 2 3)
;Value 109: #[tagged-data 109 (vector number) #(1 2 3)]

(vector-dimension (numeric-vector 1 2 3))
;Value: 3

(vector-elt (numeric-vector 1 2 3) 0)
;Value: 1

(vector-elt (numeric-vector 1 2 3) 2)
;Value: 3

(vector-elt (vector 1 2 3) 2)
;Value: 3

(get-predicate (vector 1 2 3))
;Value 110: #[compiled-procedure 110 ("vector" #xb) #x14 #x1056a6f44]

(pt (get-predicate (vector 1 2 3)))
vector
;Unspecified return value

(pt (get-predicate (numeric-vector 1 2 3)))
(vector number)
;Unspecified return value

(rewrite-tags (numeric-vector 1 2 3))
;Value 114: (tagged-data (vector number) #(1 2 3))

(eqv? numeric-vector? (get-predicate (numeric-vector 1 2 3)))
;Value: #t
