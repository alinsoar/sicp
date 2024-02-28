#lang racket

(require "sicp.rkt")
(GETMOD 2 24)
(require scheme/mpair)
(require compatibility/mlist)

(define (count-pairs x)
  (if (not (mpair? x))
      0
      (+ (count-pairs (mcar x))
         (count-pairs (mcdr x))
         1)))

"
     +---+  +1              +---+
     | o +----------------->| a |
  +1 +---+               +1 +---+
     | o +--------+         | a |
     +---+  +1    |         +---+
                  |
                  |         +---+
                  +-------->| b |
                         +1 +---+
                            | b |
                            +---+
"

;; THREE pairs
(define a3 (mcons 'a 'a))
(define b3 (mcons 'b 'b))
(define c3 (mcons a3 b3))

"
     +---+  +3     +---+  +1    +---+
     | o +-------->| o +------->| a |
  +1 +---+      +1 +---+     +1 +---+
     | o +-------->| o +------->| a |
     +---+  +3     +---+  +1    +---+
"

;; SEVEN pairs
(define a7 (mcons 'a 'a))
(define b7 (mcons a7 a7))
(define c7 (mcons b7 b7))


"
     +---+  +3     +---+  +1    +---+
     | o +-------->| o +------->| a |
  +1 +---+      +1 +---+     +1 +---+
     | c |         | o +------->| a |
     +---+         +---+  +1    +---+
"

;; FOUR pairs
(define a4 (mcons 'a 'a))
(define b4 (mcons a4 a4))
(define c4 (mcons b4 'c))

"
     +---+  +3     +---+        +---+
     | o +-------->| o +<------>| o |
  +1 +---+      +1 +---+  LOOP  +---+
     | c |         | b |        | a |
     +---+         +---+        +---+
"

;; LOOP forever
(define a* (mcons nil 'a))
(define b* (mcons a* 'b))
(set-mcar! a* b*)
(define c* (mcons b* 'c))

;Aborting!: maximum recursion depth exceeded

(module+ test
  (require rackunit)
  (require racket/sandbox)
  ".............."
  "three"
  (count-pairs c3)
  "seven"
  (count-pairs c7)
  "four"
  (count-pairs c4)
  "loop"

  (infinite-loop (lambda () (count-pairs c*))
                 "infinite loop"
                 .1
                 "exp")
  (tree-view-with-selectors-and-limit c* 30 mcar mcdr mpair? mcons)
  (tree-view-with-selectors c3 mcar mcdr mpair? mcons)
  (tree-view-with-selectors c7 mcar mcdr mpair? mcons)
  (tree-view-with-selectors c4 mcar mcdr mpair? mcons)

  )
