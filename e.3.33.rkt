#lang racket

(require "sicp.rkt")

;;; ABSTRACTIONS

(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector 'u))
        (v (make-connector 'v))
        (w (make-connector 'w))
        (x (make-connector 'x))
        (y (make-connector 'y)))
    (map probe (list u v w x y))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))
(define (averager a b c)
  (let ((x (make-connector 'x-avg))
        (y (make-connector 'y-avg)))
    (adder a b x)
    (constant .5 y)
    (multiplier x y c)))

;;; PRIMITIVE CONSTRAINTS

(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me))
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          ((eq? request 'type)
           (format "ADDER ~a ~a ~a"
                   (a1 'get-name)
                   (a2 'get-name)
                   (sum 'get-name)))
          (else
           (error "Unknown request -- ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)
(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       me))
          ((and (has-value? product) (has-value? m1))
           (set-value! m2
                       (/ (get-value product) (get-value m1))
                       me))
          ((and (has-value? product) (has-value? m2))
           (set-value! m1
                       (/ (get-value product) (get-value m2))
                       me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          ((eq? request 'type)
           (format "MULTIPLIER ~a ~a ~a"
                   (m1 'get-name)
                   (m2 'get-name)
                   (product 'get-name)))
          (else
           (error "Unknown request -- MULTIPLIER" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)
(define (constant value connector)
  (define (me request)
    (cond ((eq? request 'type)
           (format "CONSTANT ~a ~a"
                   value
                   (connector 'get-name)))
          (else (error "Unknown request -- CONSTANT" request))))
  (connect connector me)
  (set-value! connector value me)
  me)
(define (probe connector)
  (define (print-probe value)
    (display "Probe: ")
    (display (get-name connector))
    (display " = ")
    (display value)
    (newline))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          ((eq? request 'type)
           (format "PROBE ~a" (connector 'get-name)))
          (else
           (error "Unknown request -- PROBE" request))))
  (connect connector me)
  me)

;;; COMBINATIONS

(define (make-connector name)
  (let ((value false) (informant false) (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-constraint-except setter
                                         inform-about-value
                                         constraints))
            ((not (= value newval))
             (format "Error! Contradiction ~a"
                     (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant false)
                 (for-each-constraint-except retractor
                                             inform-about-no-value
                                             constraints))
          'ignored))
    (define (connect new-constraint)
      (and (not (memq new-constraint constraints))
           (set! constraints
                 (cons new-constraint constraints)))
      (and (has-value? me)
           (inform-about-value new-constraint))
      'done)
    (define (str)
      (format "CONNECTOR ~a ~a [~a] ~a"
              name
              (if informant value "?")
              (constraint-type informant)
              (map (lambda (x) (format "~a;" (x 'type)))
                   constraints)))
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant true false))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            ((eq? request 'get-name) name)
            ((eq? request 'str) str)
            (else (error "Unknown operation -- CONNECTOR"
                         request))))
    me))

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))
(define (constraint-type constraint)
  (if (procedure? constraint)
      (constraint 'type)
      constraint))

(define (has-value? connector)
  (connector 'has-value?))
(define (get-value connector)
  (connector 'value))
(define (print-connector connector)
  (void (o ((connector 'str)) "\n")))
(define (get-name connector)
  (connector 'get-name))
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))

;;; EVAL
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))
;;; APPLY
(define (for-each-constraint-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception)
           (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))

(module+ test
  (define C (make-connector "Celsius temp"))
  (define F (make-connector "Fahrenheit temp"))
  (celsius-fahrenheit-converter C F)
  (void (probe C)
        (probe F))
  "---"
  (set-value! C 25 'user)
  (set-value! F 212 'user)
  (print-connector C)
  (print-connector F)
  "---"
  (forget-value! C 'user)
  (print-connector F)
  (print-connector C)
  "----------"
  (set-value! F 212 'user)
  (define a (make-connector 'input-1))
  (define b (make-connector 'input-2))
  (define c (make-connector 'average))
  (averager a b c)
  (void (probe a)
        (probe b)
        (probe c))
  (set-value! a 10 'user)
  (set-value! b 20 'user)
  (forget-value! c 'user)
  (get-value c)
  ;; => b = 40
  (set-value! c 25 'user)

  "--"
  (define x (make-connector 'x))
  (define y (make-connector 'y))
  (define z (make-connector 'z))
  (adder x y z)
  (set-value! x 10 'user)
  (set-value! y 20 'user)
  (print-connector x)
  (print-connector y)
  (print-connector z)
  "--"
  (forget-value! x 'user)
  (print-connector x)
  (print-connector y)
  (print-connector z)
  
  )

(module+ export
  (provide multiplier
           make-connector
           probe
           set-value!
           forget-value!
           has-value?
           get-value
           connect
           print-connector
           adder
           constant
           ))

