#lang racket

(require "sicp.rkt")
(GETMOD 3 28)
(require scheme/mpair)
(require compatibility/mlist)

(define (make-lifo)
  "We get bad results using a LIFO because the propagation depends on
time. If we use a LIFO instead of a FIFO the evaluation does not take
into account the forward evaluation in time, but it evaluates backward
in time."
  (let ((data '()))
    (define (empty?) (null? data))
    (define (front)
      (if (empty?)
          (error "FRONT called with empty lifo")
          (mcar data)))
    (define (insert! item)
      (set! data (mcons item data))
      'ok)
    (define (delete!)
      (cond ((empty?) (error "DELETE! called with an empty lifo"))
            (else (set! data (mcdr data))))
      'ok)
    (define (print)
      (d "LIFO:" data))
    (define (str) (format "~a" data))
    (define (dispatch m)
      (cond ((eq? m 'insert) insert!)
            ((eq? m 'delete) delete!)
            ((eq? m 'front) front)
            ((eq? m 'get-front-ptr) data)
            ((eq? m 'empty?) empty?)
            ((eq? m 'print) print)
            ((eq? m 'str) str)
            (else "unknown message") ) )
    dispatch ) )

(define insert-lifo! (lambda (lifo x) ((lifo 'insert) x)))
(define delete-lifo! (lambda (lifo) ((lifo 'delete))))
(define empty-lifo? (lambda (lifo) ((lifo 'empty?))))
(define front-lifo (lambda (lifo) ((lifo 'front))))
(define print-lifo (lambda (lifo) ((lifo 'print))))
(define str-lifo (lambda (lifo) ((lifo 'str))))

(module+ test
  (define (test-lifo)
    (define q1 (make-lifo))
    (print-lifo q1)

    (insert-lifo! q1 'a)
    (print-lifo q1)

    (insert-lifo! q1 'b)
    (print-lifo q1)
    (d (front-lifo q1))

    (delete-lifo! q1)
    (print-lifo q1)

    (delete-lifo! q1)
    (print-lifo q1)
    'ok)
  "---------- test lifo"
  ;; (test-lifo)

  (define the-agenda (make-agenda make-lifo
                                  empty-lifo?
                                  insert-lifo!
                                  delete-lifo!
                                  front-lifo))
  "---------- define wires"
  (define a   (make-wire 'input-1 the-agenda))
  (define b   (make-wire 'input-2 the-agenda))
  (define out (make-wire 'and-output the-agenda))

  (define (propagate-and-print-status)
    (print-agenda the-agenda)
    (d ">>>")
    (propagate the-agenda)
    (void (map print-wire (list a b out))))

  "---------- instrument wires"
  (probe a b out)
  (propagate-and-print-status)

  "---------- define the and gate"
  (and-gate a b out the-agenda)
  (propagate-and-print-status)
  
  "---------- set signal 1 on both inputs"
  (set-signal! a 1)
  (set-signal! b 1)
  (propagate-and-print-status)
  
  "---------- WIRE STATUS SHOWS A FIRST ERROR"
  (void (print-wire a))
  (void (print-wire b))
  (void (print-wire out))

  "---------- set signal 0 on the 1st input"
  (set-signal! a 0)
  (propagate-and-print-status)
  
  "---------- set signal 1 on the 1st input and 0 and on the 2nd input -- ERROR"
  (set-signal! a 1)
  (set-signal! b 0)
  (propagate-and-print-status)

  "---------- WIRE STATUS SHOWS A SECOND ERROR"
  (void (print-wire a))
  (void (print-wire b))
  (void (print-wire out))
  )


