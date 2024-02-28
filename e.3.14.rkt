#lang racket

(require "sicp.rkt")
(GETMOD 2 24)
(require scheme/mpair)
(require compatibility/mlist)

"
  ... line: mutations
  --- line: initial configuration
  /// line: mutation breaks initial configuration
  (*) initial pointer of V


  LOOP:

   +----- X          ..... temp                              Y
   |            /    .                                       |
   V           /     V                                       |
 +---+---+    /    +---+---+    +---+---+    +---+---+       V
 | a | o +---/---->| b | o +--->| c | o +--->| d | / |
 +---+---+  /      +---+---+    +---+---+    +---+---+       ()
   *   .   /
  (*)  .  /
       .
       .....> y

            *

   LOOP:

   +----- X         ..... temp                          +-- Y
   |            /   .                                   |
   V           /    V                                   V
 +---+---+    /   +---+---+    +---+---+              +---+---+
 | b | o +---/--->| c | o +--->| d | / |              | a | o |
 +---+---+  /     +---+---+    +---+---+              +---+---+
       .   /                                            *
       .  /                                            (*)
       .
       .....> y


   LOOP:

   +----- X         ..... temp            +-- Y
   |            /   .                     |
   V           /    V                     V
 +---+---+    /   +---+---+             +---+---+     +---+---+
 | c | o +---/--->| d | o +             | b | / +---->| a | o |
 +---+---+  /     +---+---+             +---+---+     +---+---+
       .   /                                            *
       .  /                                            (*)
       .
       .....> y


   LOOP:

   +----- X                               +-- Y
   |   ........ temp                      |
   V   V                                  V
 +---+---+                +---+---+     +---+---+     +---+---+
 | d | / |                | c | o +---->| b | / +---->| a | o |
 +---+---+                +---+---+     +---+---+     +---+---+
       .                                                *
       .....> y                                        (*)




   LOOP:

   X           +-- Y
   |           |
   V           V
             +---+---+    +---+---+     +---+---+     +---+---+
   ()        | d | / +--->| c | o +---->| b | / +---->| a | o |
             +---+---+    +---+---+     +---+---+     +---+---+
                                                        *
                                                       (*)
"


(define (mystery x)
  (define (loop x y)
    (d "." x "//" y)
    (if (null? x)
        y
        (let ((temp (mcdr x)))
          (set-mcdr! x y)
          (loop temp x))))
  (loop x '()))

(module+ test

  (define v (mlist 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j 'k))

  (define w (mystery v))

  (tree-view-with-selectors w mcar mcdr mpair? mcons)
  (tree-view-with-selectors v mcar mcdr mpair? mcons)
  )
