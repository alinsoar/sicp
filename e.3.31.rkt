#lang racket

(require "sicp.rkt")
(GETMOD 3 28)
(GETMOD 3 22 q:)

(require scheme/mpair)
(require compatibility/mlist)
 
(define double-negation
  (lambda (a not-a out agenda)
    (inverter a not-a agenda)
    (inverter not-a out agenda)
    'done))
(define half-adder
  (lambda (a b s c agenda populate-agenda?)
    (let ((d (make-wire 'd-ha agenda populate-agenda?))
          (e (make-wire 'e-ha agenda populate-agenda?)))
      (probe d e)
      (or-gate a b d agenda)
      (and-gate a b c agenda)
      (inverter c e agenda)
      (and-gate d e s agenda)
      'ok)))

(module+ test
  (define the-agenda (make-agenda q:make-queue
                                  q:empty-queue?
                                  q:insert-queue!
                                  q:delete-queue!
                                  q:front-queue))
  
  (define (test-double-negation populate-agenda?)
    (d "****************************************")
    (define a         (make-wire 'a   the-agenda populate-agenda?))
    (define not-a     (make-wire 'a_  the-agenda populate-agenda?))
    (define not-not-a (make-wire 'a__ the-agenda populate-agenda?))

    (d "---------- make double negation abstraction")
    (double-negation a not-a not-not-a the-agenda)
    (d "---------- instruement the main wires")
    (probe a not-a not-not-a)
    (d "---------- print agenda")
    (print-agenda the-agenda)
    (d ">>>")
    (propagate the-agenda)
    (d "---------- set signal on a to 1")
    (set-signal! a 1)
    (print-agenda the-agenda)
    (d ">>>")
    (propagate the-agenda)
    (d (map print-wire (list a not-a not-not-a)))
    'done)

  (define (test-half-adder populate-agenda?)
    (o "V^V^V^V^V^V^V^V^V^V^V^V^V^V^V^V^V^V^V^V^"
       "V^V^V^V^V^V^V^V^V^V^V^V^V^V^V^V^V^V^V^V\n")
    (reset-agenda the-agenda)
    (print-agenda the-agenda)
    (define a (make-wire 'a the-agenda populate-agenda?))
    (define b (make-wire 'b the-agenda populate-agenda?))
    (define s (make-wire 's the-agenda populate-agenda?))
    (define c (make-wire 'c the-agenda populate-agenda?))
    
    (define (propagate-and-print)
      (print-agenda the-agenda)
      (d ">>>")
      (propagate the-agenda)
      (map print-wire (list a b c s))
      )
    (d "---------- instrument wires")
    (probe a b c s)
    (propagate-and-print)
    (d "---------- define the HALF ADDER")
    (half-adder a b s c the-agenda populate-agenda?)
    (propagate-and-print)
    (d "---------- SET signal on a to 1")
    (set-signal! a 1)
    (propagate-and-print)
    (d "---------- SET signal on b to 1")
    (set-signal! b 1)
    (propagate-and-print)
    (d "---------- SET signal on a AND b to 0")
    (set-signal! a 0)
    (set-signal! b 0)
    (propagate-and-print)
    (d "---------- SET signal on b to 1")
    (set-signal! b 1)
    (propagate-and-print)
    (d "---------- WIRE STATUS")
    (map print-wire (list a b s c))
    'done)

  ;; The test shows that if we do not re-initialize the network after
  ;; we add any wire, the network could remain in an inconsistent
  ;; state. 
  
  ;; in the case of double negation, after initialization we should
  ;; have had the wire a_ with signal 1. When we try to set the signal
  ;; to 1 the agenda is not populated correctly, because the
  ;; set-signal! thinks that the signal is already set correctly, and
  ;; it stops propagating further.
  
  ;; in the case of the half adder, the agenda is empty after
  ;; initialization, and (propagate) does not manage to set the wire
  ;; e-ha to 1. Consequently, when we try to set the signal on a to 1,
  ;; the sum is not correctly updated, and the network remains in an
  ;; inconsistent state.
  
  ;; "--- DOUBLE-NEGATION ; POPULATE"     (test-double-negation true)
  ;; "--- DOUBLE-NEGATION ; NOT POPULATE" (test-double-negation false)
  "--- HALF ADDER ; POPULATE"          (test-half-adder true)
  ;; "--- HALF ADDER ; NOT POPULATE"      (test-half-adder false)
  )
