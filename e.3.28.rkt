#lang racket

(require "sicp.rkt")
(GETMOD 3 22 q:)

(require scheme/mpair)
(require compatibility/mlist)

;;; Implementing AGENDA -- EVAL AND APPLY are in the agenda.

(define make-agenda
  (lambda (make-queue empty-queue? insert-queue! delete-queue! front-queue)
    (let ((current-agenda-time 0) (agenda-segments (mlist)))

      ;; AGENDA SEGMENTS
      (define (make-time-segment time queue) (mcons time queue))
      (define (segment-time s) (mcar s))
      (define (segment-queue s) (mcdr s))

      ;; AGENDA SCHEDULER
      (define (current-time) current-agenda-time)
      (define (set-current-time! time) (set! current-agenda-time time))
      (define (segments) agenda-segments)
      (define (set-segments! segments) (set! agenda-segments segments))
      (define (first-segment) (mcar (segments)))
      (define (rest-segments) (mcdr (segments)))
      (define (reset)
        (set! current-agenda-time 0)
        (set! agenda-segments (mlist)))

      (define (print-agenda)
        (define (print-queue q)
          "this procedure uses the internal structure of a queue from
module e.3.22.rkt. It cannot be used on other implementation of a
queue with different structure."
          (define (iter l)
            (cond ((null? l) "")
                  (else (format "~a ; ~a"
                                ((mcar l) 'data)
                                (iter (mcdr l))))))
          (if (empty-queue? q)
              "EMPTY"
              (iter (q 'get-front-ptr))))
        (define format-output
          (format "AGENDA TIME: ~a\n~a"
                  (current-time)
                  (if (empty-agenda?)
                      "EMPTY AGENDA\n"
                      (reduce (lambda (s r)
                                (format "SEG TIME ~a ACTIONS ~a\n~a"
                                        (segment-time s)
                                        (print-queue (segment-queue s))
                                        r))
                              ""
                              (segments)))))
        (o format-output))
      (define (empty-agenda?) (null? (segments)))
      (define (add-to-agenda! time action)
        "ACTION is one of: OR-PROPAGATOR, AND-PROPAGATOR, NOT-PROPAGATOR or PROBE"
        (define (belongs-before? segments)
          (or (null? segments)
              (< time (segment-time (mcar segments)))))
        (define (make-new-time-segment time action)
          (let ((q (make-queue)))
            (insert-queue! q action)
            (make-time-segment time q)))
        (define (add-to-segments! segments)
          (if (= (segment-time (mcar segments)) time)
              (insert-queue! (segment-queue (mcar segments))
                             action)
              (let ((rest (mcdr segments)))
                (if (belongs-before? rest)
                    (set-mcdr!
                     segments
                     (mcons (make-new-time-segment time action)
                            (mcdr segments)))
                    (add-to-segments! rest)))))
        (let ((segments (segments)))
          (if (belongs-before? segments)
              (set-segments!
               (mcons (make-new-time-segment time action)
                      segments))
              (add-to-segments! segments))))
      (define (remove-first-agenda-item!)
        (let ((q (segment-queue (first-segment))))
          (delete-queue! q)
          (and (empty-queue? q)
               (set-segments! (rest-segments)))))
      (define (first-agenda-item)
        (if (empty-agenda?)
            (error "Agenda is empty -- FIRST-AGENDA-ITEM")
            (let ((first-seg (first-segment)))
              (set-current-time! (segment-time first-seg))
              (front-queue (segment-queue first-seg)))))
      (define (after-delay delay action)
        "this is the EVAL function"
        (add-to-agenda! (+ delay (current-time))
                        action))
      (define (propagate)
        "this is the APPLY function"
        (if (empty-agenda?)
            'done
            (let ((first-item (first-agenda-item)))
              ;; each item is a propagator for a gate, that may add
              ;; new propagators to the agenda, depending on what
              ;; gates are activated and which signals they receive.
              (first-item)
              (remove-first-agenda-item!)
              (propagate))))

      (define (dispatch message)
        (cond (false 'ok)
              ((eq? message 'after-delay) after-delay)
              ((eq? message 'current-time) current-time)
              ((eq? message 'print) print-agenda)
              ((eq? message 'propagate) propagate)
              ((eq? message 'reset) reset)
              ))
      dispatch)))

;;; in all our experiments only 1 agenda is enough
(define (after-delay delay action agenda) ((agenda 'after-delay) delay action))
(define (current-time agenda) ((agenda 'current-time)))
(define (print-agenda agenda) ((agenda 'print)))
(define (reset-agenda agenda) ((agenda 'reset)))
(define (propagate agenda) ((agenda 'propagate)))

;;; Implementing ABSTRACTIONS

(define (half-adder a b s c agenda)
  (let ((d (make-wire 'd-ha agenda)) (e (make-wire 'e-ha agenda)))
    (probe d e)
    (or-gate a b d agenda)
    (and-gate a b c agenda)
    (inverter c e agenda)
    (and-gate d e s agenda)
    'ok))
(define (full-adder a b c-in sum c-out agenda)
  (let ((s (make-wire 's agenda))
        (c1 (make-wire 'c1 agenda))
        (c2 (make-wire 'c2 agenda)))
    (half-adder b c-in s c1 agenda)
    (half-adder a s sum c2 agenda)
    (or-gate c1 c2 c-out agenda)
    'ok))

;;; Implementing LOGICAL GATES -- PRIMITIVE OBJECTS

(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay  5)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))
(define (logical-and x y)
  (if (and (= x 1) (= y 1))
      1
      0))
(define (logical-or x y)
  (if (and (= x 0) (= y 0))
      0
      1))
(define (logical-nand x y)
  (if (and (= x 1) (= y 1))
      0
      1))

(define (connector input output agenda)
  (define (connect-action . get-data)
    "CONNECT-ACTION lies on wire in ACTION-PROCEDURES and, when called,
it inserts a propagator in agenda that sends a signal on OUTPUT when
the INPUT wire is activated and its actions are executed. The
propagator will send to output the same signal from input."
    (let* ((new-value (get-signal input))
           (str (format "CONNECT ~a ~a =>~a"
                        (input 'get-name) (output 'get-name)
                        new-value)))
      (if (null? get-data)
          (after-delay inverter-delay
                       (lambda data
                         (if (null? data)
                             (set-signal! output new-value)
                             str))
                       agenda)
          (string-append "!" str))))
  (add-action! input connect-action)
  'ok)
(define (inverter input output agenda)
  (define (invert-input-action . get-data)
    "INVERT-INPUT-ACTION lies on wire in ACTION-PROCEDURES and, when called,
it inserts a propagator in agenda that sends a signal on OUTPUT when
the INPUT wire is activated and its actions are executed. The
propagator will send to the output the logical negation of the signal
from input."
    (let* ((new-value (logical-not (get-signal input)))
           (str (format "NOT ~a ~a =>~a"
                        (input 'get-name) (output 'get-name)
                        new-value)))
      (if (null? get-data)
          (after-delay inverter-delay
                       (lambda data
                         (if (null? data)
                             (set-signal! output new-value)
                             str))
                       agenda)
          (string-append "!" str))))
  (add-action! input invert-input-action)
  'ok)
(define (or-gate a1 a2 output agenda)
  (define (or-action . get-data)
    "OR-ACTION lies on wire in ACTION-PROCEDURES and, when called,
it inserts a propagator in agenda that sends a signal on OUTPUT when
any of the input wires A1 or A2 is activated and its actions are
executed. The propagator will send to output the logical OR of the
signals from the input wires A1 and A2."
    (let* ((new-value (logical-or (get-signal a1) (get-signal a2)))
           (str (format "OR ~a ~a ~a =>~a"
                        (a1 'get-name) (a2 'get-name) (output 'get-name)
                        new-value)))
      (if (null? get-data)
          (after-delay or-gate-delay
                       (lambda data
                         (if (null? data)
                             (set-signal! output new-value)
                             str))
                       agenda)
          (string-append "!" str))))
  (add-action! a1 or-action)
  (add-action! a2 or-action)
  'ok)
(define (and-gate a1 a2 output agenda)
  (define (and-action . get-data)
    "AND-ACTION lies on wire in ACTION-PROCEDURES and, when called,
it inserts a propagator in agenda that sends a signal on OUTPUT when
any of the input wires A1 or A2 is activated and its actions are
executed. The propagator will send on output wire the logical AND of
the signals from inputs A1 and A2."
    (let* ((new-value (logical-and (get-signal a1) (get-signal a2)))
           (str (format "AND ~a ~a ~a =>~a"
                        (a1 'get-name) (a2 'get-name) (output 'get-name)
                        new-value)))
      (if (null? get-data)
          (after-delay and-gate-delay
                       (lambda data
                         (if (null? data)
                             (set-signal! output new-value)
                             str))
                       agenda)
          (string-append "!" str))))
  (add-action! a1 and-action)
  (add-action! a2 and-action)
  'ok)
(define (sheffer-stroke-gate a1 a2 output agenda)
  (define (sheffer-stroke-action . get-data)
    "This is the NAND operator, which alone is functionally complete
for boolean algebra."
    (let* ((new-value (logical-nand (get-signal a1) (get-signal a2)))
           (str (format "NAND ~a ~a ~a =>~a"
                        (a1 'get-name) (a2 'get-name) (output 'get-name)
                        new-value)))
      (if (null? get-data)
          (after-delay and-gate-delay
                       (lambda data
                         (if (null? data)
                             (set-signal! output new-value)
                             str))
                       agenda)
          (string-append "!" str))))
  (add-action! a1 sheffer-stroke-action)
  (add-action! a2 sheffer-stroke-action)
  'ok)

;;; Implementing WIRES -- MEANS OF COMBINATION

(define (make-wire name agenda . populate-agenda?)
  (let ((signal-value 0) (action-procedures (mlist)))
    (define (str-wire)
      (format "WIRE ~a SIGNAL VALUE ~a ACTIONS ~a"
              name
              signal-value
              (reduce
               string-append
               ""
               (mmap (lambda (a) (format "~a;" (a 'data)))
                     action-procedures))))
    (define (print-wire)
      (o (str-wire) "\n"))
    (define (install-probe)
      (define (probe . get-data)
        "`probe` lies only on the wire in `action-procedures` and it
installs nothing into the agenda, so it does propagate nothing when
the wire is activated and call-actions are executed."
        (let ((format
               (lambda (s)
                 (format s name (current-time agenda) signal-value))))
          (if (null? get-data)
              (o (format ":~a ~a # New-value = ~a\n"))
              (format "!PROBE ~a ~a / ~a"))))
      (ACCEPT-ACTION-PROCEDURE! probe))
    (define (SET-SIGNAL! new-value)
      "Signal value is set either by user, or by some propagator, when
it is scheduled for execution from agenda."
      (define (call-each procedures)
        (if (null? procedures)
            'done
            (begin
              ((mcar procedures))
              (call-each (mcdr procedures)))))
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (ACCEPT-ACTION-PROCEDURE! proc)
      (o (format "!INSTALL ACTION [~a] on WIRE ~a\n" (proc 'data) name))
      (set! action-procedures (mcons proc action-procedures))
      ;; Via ACCEPT_ACTION_PROCEDURE, we attach the procedure `proc`
      ;; to wire's actions list, to be executed each time a wake-up
      ;; happens on the wire.
      ;;
      ;; PROC must be either a primitive logic gate or a probe.
      ;;
      ;; A PLG, when activated, will install a propagator in the
      ;; agenda. The propagator of a logic gate is a lambda expression
      ;; that knows the state of that logic gate, can get the value of
      ;; signals on its inputs, and can set the value of the output
      ;; wire.
      ;;
      ;; The signal on the output wire is set by a propagator when it
      ;; is scheduled for execution in agenda.
      ;;
      ;; When scheduled for execution, the propagator of a PLG calls
      ;; SET-SIGNAL! on the output of its logic gate. SET-SIGNAL!
      ;; checks whether the output wire signal changes the value and,
      ;; if so, it will set the new value and call all its actions.
      ;; Each action will either print a probe, or install a new
      ;; propagator in agenda, this time for output wire. Like that,
      ;; the signal will propagate further and further all over the
      ;; network, until no change occurs when SET-SIGNAL checks the
      ;; new and old value.
      ;;
      ;; if populate-agenda? is not void, it will install the action
      ;; on the wire, but it will not populate the agenda with the
      ;; propagaor defined by this action.
      (and (or (null? populate-agenda?)
               (car populate-agenda?))
           (begin (o "!RUN ACTION: " (proc 'data) "\n")
                  (proc))))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) SET-SIGNAL!)
            ((eq? m 'add-action!) ACCEPT-ACTION-PROCEDURE!)
            ((eq? m 'get-actions) action-procedures)
            ((eq? m 'get-name) name)
            ((eq? m 'print) print-wire)
            ((eq? m 'probe) install-probe)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))

(define (print-wire wire)
  ((wire 'print)))
(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))
(define probe
  (lambda wires
    (map (lambda (wire) ((wire 'probe)))
         wires)
    'done))

(define (reduce op init l)
  (if (null? l)
      init
      (op (mcar l)
          (reduce op init (mcdr l)))))

(module+ test
  (define the-agenda (make-agenda q:make-queue
                                  q:empty-queue?
                                  q:insert-queue!
                                  q:delete-queue!
                                  q:front-queue))  
  (define input-1 (make-wire 'input-1 the-agenda))
  (define input-2 (make-wire 'input-2 the-agenda))
  (define sum (make-wire 'sum the-agenda))
  (define carry (make-wire 'carry the-agenda))
  ""
  (define (propagate-and-print-status)
    (d "---")
    (print-agenda the-agenda)
    (propagate the-agenda)
    (map print-wire (list input-1 input-2 sum carry))
    'ok)
  "---------- INSTRUMENT WIRES ----------"
  (probe sum carry input-1 input-2)
  (propagate-and-print-status)
  "---------- DEFINE A HALF-ADDER ----------"
  (half-adder input-1 input-2 sum carry the-agenda)
  (propagate-and-print-status)
  "---------- SET 1 ON CARRY ----------"
  (set-signal! carry 1)
  "It is correct to see ``NOT carry e-ha =>0``"
  (propagate-and-print-status)
  "---------- SET 0 ON CARRY ----------"
  (set-signal! carry 0)
  (propagate-and-print-status)
  "---------- SET 1 ON A WIRE ENTERING THE HALF ADDER ----------"
  (set-signal! input-1 1)
  (propagate-and-print-status)
  "---------- SET 1 ON THE OTHER WIRE ENTERING THE HALF ADDER ----------"
  (set-signal! input-2 1)
  (propagate-and-print-status)
  "---------- SET 0 ON BOTH INPUTS OF THE HALF ADDER ----------"
  (set-signal! input-1 0)
  (set-signal! input-2 0)
  (propagate-and-print-status)
  )

(module+ export
  (provide make-wire
           inverter
           and-gate
           or-gate
           sheffer-stroke-gate
           logical-or
           make-agenda
           or-gate-delay
           after-delay
           probe
           get-signal
           set-signal!
           print-agenda
           current-time
           reset-agenda
           add-action!
           connector
           print-wire
           full-adder
           propagate))
