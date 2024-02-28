#lang racket

(require "sicp.rkt")
(GETMOD 2 24)
(require scheme/mpair)
(require compatibility/mlist)

"
        +---+---+
  z1--->| o | o |
        +-+-+-+-+
          |   |
          |   |
          V   V
        +---+---+      +---+---+
  x --->| o | o +----->| o | / |
        +---+---+      +---+---+
          |              |
 replace  a              b
 by      WOW
                                 WOW
                                  |
        +---+---+               +---+---+      +---+---+
  z1--->| o | o +-------------->| o | o +----->| o | / |
        +-+-----+               +---+---+      +---+---+
          |            (cut link) /              |
          |                       a              b
          |                       |              |
          |                     +---+---+      +---+---+
          +-------------------->| o | o +----->| o | / |
                                +---+---+      +---+---+
"

(define x (mlist 'a 'b))
(define z1 (mcons x x))
(define z2 (mcons (mlist 'a 'b) (mlist 'a 'b)))

(define (set-to-wow! x)
  (set-mcar! (mcar x) 'wow)
  x)

(module+ test
  "---"
  z1
  (set-to-wow! z1)
  z2
  (set-to-wow! z2)
  "---"
  (tree-view-with-selectors z1 mcar mcdr mpair? mcons)
  (tree-view-with-selectors z2 mcar mcdr mpair? mcons)
  )
