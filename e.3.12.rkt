#lang racket

(require "sicp.rkt")
(GETMOD 2 24)
(require scheme/mpair)
(require compatibility/mlist)

;;;                         append! modifies the NIL here
;;; x --+  ...z             append duplicates x and keep NIL here
;;;     |  .                                |
;;;     V  V                                V
;;;  +--+----+-------+         +-------+-------+
;;;  |   o   |    o  |-------->|   o   |   ()  |......
;;;  +---+---+-------+         +---+---+-------+     .
;;;      |                         |                 .
;;;      V                         V                 .
;;;      a                         b                 .
;;;                                                  .
;;;                        z=(append x y)            .
;;;  y --+  ..........................................
;;;      |  .
;;;      V  V
;;;   +--+----+-------+         +-------+-------+
;;;   |   o   |    o  |-------->|   o   |    /  |
;;;   +---+---+-------+         +---+---+-------+
;;;       |                         |
;;;       V                         V
;;;       c                         d


(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

(define (append! x y)
  (set-mcdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (mcdr x))
      x
      (last-pair (mcdr x))))

(module+ test

  (define x (list 'a 'b))
  (define y (list 'c 'd))
  (define z (append  x y))
  (tree-view x)
  (tree-view y)
  (tree-view z)
  (tree-view (cdr x))
  "---"
  (define xm (mlist 'a 'b))
  (define ym (mlist 'c 'd))
  (define wm (append! xm ym))
  (tree-view (mlist->list (mcdr xm)))
  (tree-view (mlist->list wm))
  (set-mcar! ym 'change)
  (tree-view-with-selectors wm mcar mcdr mpair? mcons)
  )
