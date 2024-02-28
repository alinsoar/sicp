#lang racket

(require "sicp.rkt")
(GETMOD 3 22 q:)
(GETMOD 3 28 without or-gate)

;;;                    artist-mode: logical OR defined using de-Morgan's law
;;;             |\                  +------+
;;;       a1    |  \      a1-i      |       \
;;;     --------|NOT}---------------+        \
;;;             |  /                |         \               |\
;;;             |/                  |          \    and-out   |N \    output
;;;                                 |  AND      }-------------| O }------------
;;;             |\                  |          /              |T /
;;;       a2    |  \      a2-i      |         /               |/
;;;     --------|NOT}---------------+        /
;;;             |  /                |       /
;;;             |/                  +------+

(define (or-gate a1 a2 output agenda)
  (let ((a1-i (make-wire 'a1-i agenda))
        (a2-i (make-wire 'a2-i agenda))
        (and-out (make-wire 'and-out agenda)))
    (probe a1-i a2-i and-out)
    (inverter a1 a1-i agenda)
    (inverter a2 a2-i agenda)
    (and-gate a1-i a2-i and-out agenda)
    (inverter and-out output agenda))
  'ok)

(define (sheffer-stroke-or-gate a1 a2 output agenda)
  "a 2nd method, in which OR gate is defined in function of NAND,
which is a functional combination of NOT and AND."
  (let ((a1-nand (make-wire 'a1.nand agenda))
        (a2-nand (make-wire 'a2.nand agenda)))
    (probe a1-nand a2-nand)
    (sheffer-stroke-gate a1 a1 a1-nand agenda)
    (sheffer-stroke-gate a2 a2 a2-nand agenda)
    (sheffer-stroke-gate a1-nand a2-nand output agenda)))

(module+ test
  (define the-agenda (make-agenda q:make-queue
                                  q:empty-queue?
                                  q:insert-queue!
                                  q:delete-queue!
                                  q:front-queue))
  "****************************************"
  (define a (make-wire 'a the-agenda))
  (define b (make-wire 'b the-agenda))
  (define out-or (make-wire 'out-or the-agenda))
  (define sheffer-a (make-wire 'shefferA the-agenda))
  (define sheffer-b (make-wire 'shefferB the-agenda))
  (define sheffer-out-or (make-wire 'shefferOUT-OR the-agenda))
  (define (propagate-and-print-status wires)
    (print-agenda the-agenda)
    (d ">>>")
    (propagate the-agenda)
    (map print-wire wires)
    'done)
  (define (test gate a b out)
    (d "---------- Instrument the main (list a b out)")
    (probe a b out)
    (propagate-and-print-status (list a b out))
    (d "---------- define the or-gate")
    (gate a b out the-agenda)
    (propagate-and-print-status (list a b out))
    (d "---------- send signal '1 on wire 'a")
    (set-signal! a 1)
    (propagate-and-print-status (list a b out))
    (d "---------- send signal '0 on wire 'a")
    (set-signal! a 0)
    (propagate-and-print-status (list a b out))
    (d "---------- send signal '1 on (list a b out) 'a and 'b")
    (set-signal! a 1)
    (set-signal! b 1)
    (propagate-and-print-status (list a b out))
    (d "---------- send signal '0 on wire 'b")
    (set-signal! b 0)
    (propagate-and-print-status (list a b out))
    (d "---------- send signal '0 on wire 'a")
    (set-signal! a 0)
    (propagate-and-print-status (list a b out)))
  (test or-gate                a         b         out-or)
  (test sheffer-stroke-or-gate sheffer-a sheffer-b sheffer-out-or)
  )
