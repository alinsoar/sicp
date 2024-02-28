#lang racket

(require "sicp.rkt")

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))
(define (make-mutex)
  (let ((s (make-semaphore 1))
        (test-counter 1))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (set! test-counter (sub1 test-counter))
             (semaphore-wait s))
            ((eq? m 'release)
             (set! test-counter (add1 test-counter))
             (and (> test-counter 1) (error "mutex error" test-counter))
             (semaphore-post s))))
    the-mutex))

(define ID
  (lambda (u)
    "Delayed identity function. This function is crucial for
discovering quickly the possible output values in different
circumstances. Useful after read, before and after set, and at the
end/beginning of evalutation of an expression, to allow other
expressions to be evaluated before/after the given expression."
    (lambda (x)
      (sleep (u))
      x)))

(define default-heuristics
  (lambda (m)
    "this is a heuristics that works fast in many cases. if a
heursitics lenghtens a test more than 1 second, then we should look
for another heuristics OR inserted more delayed identities."
    (lambda () (let ((r (random)))
                 (if (> .5 r) (/ r 30000) 0)))))

(define parallel-execute
  (lambda (process-group)
    "Receives a PROCESS-GROUP object, and concurrently executes the
processes defined in the given group. Repeat the execution in many
rounds until the expected number of output values is discovered, or a
limit number of rounds is reached.

In each round all the processes from the group are executed. After
each process executes, it waits for a signal to restart. The master
thread (loop) restarts all processes at the beginning of the next
round.

At the end of each round the found value is sent to output. The output
thread keeps a list of all discovered values. If the expected number
of values is there, the output thread informs the main thread that it
must finish. If the value found at the end of a round is not in the
list of values the output thread adds the new value in the
list and prints it. Otherwise, it ignores the value."
    
    (define (reset!) ((process-group 'reset!)))
    (define (get-value) ((process-group 'get-value)))
    (define (get-expected-values) (process-group 'expected-values))
    (define (processes-list) (process-group 'processes))
    (define (restart-all-processes) (process-group 'restart-all-processes))
    (define (number-of-processes) (process-group 'count-processes))

    (define (barrier-syncronization-wait-next-round)
      (process-group 'syncro-wait))
    (define (barrier-syncronization-start-next-round)
      (process-group 'syncro-start-next-round))

    (define ch (make-channel))

    (define make-thread-for-process
      (lambda (process)
        (thread
         (lambda ()
           (let loop ()
             (barrier-syncronization-wait-next-round)
             (process 'run)
             (channel-put ch (process 'get-id))
             (process 'wait)
             (loop))))))

    (define output-thread
      (thread
       (lambda ()
         (let ((values '()))
           (define (found-all-expected-values?)
             (= (get-expected-values)
                (length values)))
           (let loop ()
             (match (thread-receive)
               ((? (lambda (x) (or (number? x)
                                   (pair? x)))
                   expr)
                (if (member expr values)
                    'already-there
                    (begin
                      (printf ":~a~n" expr)
                      (set! values (cons expr values))))
                (and (found-all-expected-values?)
                     (begin (thread-send output-thread 'done)
                            (channel-put ch 'stop)))
                (loop))
               ('done (printf "Close Output~n"))
               (_ (loop))))
           (define print-result
             (lambda (l co)
               (cond ((null? l) (co ""))
                     (else (print-result (cdr l)
                                         (lambda (x)
                                           (co (~a x " " (car l)))))))))
           (printf (print-result
                    values
                    (lambda (x)
                      (format "Values [total ~a]~a~n" (length values) x)
                      )))))))

    (define threads (map make-thread-for-process (processes-list)))

    (define finished-execution (make-vector (number-of-processes) false))

    (define found-all-expected-values false)
    (define loop-again true)

    (define (loop round-counter)
      (let ((value (channel-get ch)))
        (cond ((equal? value 'proc1) (vector-set! finished-execution 0 true))
              ((equal? value 'proc2) (vector-set! finished-execution 1 true))
              ((equal? value 'proc3) (vector-set! finished-execution 2 true))
              ((equal? value 'proc4) (vector-set! finished-execution 3 true))
              ((equal? value 'proc5) (vector-set! finished-execution 4 true))
              ((equal? value 'proc6) (vector-set! finished-execution 5 true))
              ((eq? value 'stop)     (set! found-all-expected-values true))))
      (and (not (vector-member false finished-execution))
           ;; this is a BARRIER SYNCRONIZATION POINT
           (if (and (not found-all-expected-values)
                    (positive? round-counter))
               (begin
                 ;; restart the next round
                 (vector-fill! finished-execution false)
                 (thread-send output-thread (get-value))
                 (reset!)
                 (restart-all-processes)
                 (barrier-syncronization-start-next-round))
               (begin
                 ;; kill all the threads only after we reach the
                 ;; syncronization barrier point.
                 (reset!)
                 (or found-all-expected-values
                     (thread-send output-thread 'done))
                 (thread-wait output-thread)
                 (map kill-thread threads)
                 (o "=="
                    (cond 
                     (found-all-expected-values
                      (format "found all ~a expected values~n"
                              (get-expected-values)))
                     ((negative? (get-expected-values))
                      (format "finished looping ~a times~n"
                              (- (get-expected-values))))
                     ((positive? (get-expected-values))
                      (format "Error, not found ~a different values.~n"
                              (get-expected-values)))))
                 (set! loop-again false))))
      (and loop-again
           (loop (sub1 round-counter))))
    (loop (if (< (get-expected-values) 0)
              (sub1 (* (number-of-processes)
                       (- (get-expected-values))))
              10000))))

(define make-process-list
  (lambda processes
    (or (< (length processes) 7) (error "too many processes"))
    (define (iter p N co)
      (if (null? p)
          (co '())
          (iter (cdr p)
                (add1 N)
                (lambda (x)
                  (co (cons (make-process
                             (car p)
                             (string->symbol (~a 'proc N)))
                            x))))))
    (iter processes 1 (lambda (x) x))))

(define make-process-group
  (lambda (heuristics expected-values processes reset get-value)
    ""
    (define count-processes (length processes))
    (define syncro (make-semaphore count-processes))
    (define (release-all-processes)
      (for-each (lambda (p) (p 'release))
                (shuffle processes)))
    (define (barrier-syncro-wait)
      (semaphore-wait syncro))
    (define (barrier-syncro-restart-next-round)
      (for-each (lambda (_) (semaphore-post syncro))
                processes))
    (define dispatch
      (lambda (message)
        (cond ((eq? message 'reset!) reset)
              ((eq? message 'count-processes) count-processes)
              ((eq? message 'get-value) get-value)
              ((eq? message 'expected-values) expected-values)
              ((eq? message 'processes) processes)
              ((eq? message 'syncro-wait) (barrier-syncro-wait))
              ((eq? message 'syncro-start-next-round)
               (barrier-syncro-restart-next-round))
              ((eq? message 'restart-all-processes)
               (release-all-processes))
              (else (error "unknown message -- PROCESS GROUP")))))
    dispatch))

(define make-process
  (lambda (procedure id)
    (define s (make-semaphore 0))
    (define (dispatch message)
      (cond ((eq? message 'run) (procedure))
            ((eq? message 'get-id) id)
            ((eq? message 'release) (semaphore-post s))
            ((eq? message 'wait) (semaphore-wait s))
            (else (error "unknow message -- PROCESS" message))))
    dispatch))

(module+ test

  (define default-x-value 10)

  (define (process-test1)
    (d "*** THIS TEST is 3.39")
    (define heuristics default-heuristics)
    (define expected-values 4)
    (define x default-x-value)
    (define s (make-serializer))
    (define id~
      (lambda (m v)
        (let ((DEBUG #f)) (and DEBUG (d "." m x v)))
        ((ID (heuristics m)) v)))
    (define (reset) (set! x default-x-value))
    (define (get-value) x)

    (define process-list
      (make-process-list
       (lambda ()
         (id~ '_ '_)
         (id~ 'a ((s (lambda () (id~ 'b (set! x (id~ 'c (+ x 1)))))))))
       (lambda ()
         (id~ '_ '_)
         (id~ 'd (set! x (id~ 'e ((s (lambda () (id~ 'f (* x x)))))))))))

    (make-process-group heuristics expected-values process-list reset get-value)
    )

  (define (process-test3)
    (define expected-values 2)
    (define heuristics default-heuristics)
    (define id~
      (lambda (m v)
        (let ((DEBUG #f)) (and DEBUG (d "." m x v)))
        ((ID (heuristics m)) v)))
    (define x default-x-value)
    (define s (make-serializer))
    (define (get-value) x)
    (define (reset) (set! x default-x-value))

    (define process-list
      (make-process-list
       (lambda ()
         (id~ '_ '_1)
         (id~ 'a ((s (lambda ()
                       (id~ 'b (set! x (id~ 'c (+ x 1)))))))))
       (lambda ()
         (id~ '_ '_2)
         (id~ 'd ((s (lambda ()
                       (id~ 'e (set! x (id~ 'f (* (id~ 'g x) (id~ 'h x))))))))))))
    
    (make-process-group heuristics expected-values process-list reset get-value))

  (define (process-test4)
    (define expected-values 5)
    (define heuristics
      (lambda (m)
        (lambda ()
          (let ((r (random)))
            (cond
             ((> r .5) (/ (random) 100))
             ((eq? m 'a) (/ (random) 3000))
             ((eq? m 'b) (/ (random) 3000))
             ((eq? m 'c) (/ (random) 3000))
             ((eq? m 'd) (/ (random) 3000))
             ((eq? m 'e) (/ (random) 3000))
             ((eq? m 'f) (/ (random) 3000))
             ((eq? m 'g) (/ (random) 3000))
             (else (/ (random) 1000)))))))

    (define id~
      (lambda (m v)
        (let ((DEBUG #f)) (and DEBUG (d "." m x v)))
        ((ID (heuristics m)) v)))
    (define x default-x-value)
    (define s (make-serializer))
    (define (get-value) x)
    (define (reset) (set! x default-x-value))

    (define process-list
      (make-process-list
       (lambda ()
         (id~ '_ '_) (id~ 'a (set! x (id~ 'b (+ x 1)))))
       (lambda ()
         (id~ '_ '_) (id~ 'd (set! x (id~ 'e (* (id~ 'f x) (id~ 'g x))))))))

    (make-process-group heuristics expected-values process-list reset get-value))

  (define (process-test5)
    (define heuristics default-heuristics)
    (define expected-values 5)

    (define id~
      (lambda (m v)
        (let ((DEBUG #f)) (and DEBUG (d "." m x v)))
        ((ID (heuristics m)) v)))
    (define (id v) (id~ '_ v))
    (define x default-x-value)
    (define s (make-serializer))
    (define (get-value) x)
    (define (reset) (set! x default-x-value))

    (define process-list
      (make-process-list
       (lambda ()
         (id '_)
         (id (set! x (* (id x) (id x)))))
       (lambda ()
         (id '_)
         (id (set! x (* (id x) (id x) (id x)))))))

    (make-process-group heuristics expected-values process-list reset get-value))

  (define (process-test6)
    (define heuristics default-heuristics)
    (define expected-values 1)

    (define id~
      (lambda (m v)
        (let ((DEBUG #f)) (and DEBUG (d "." m x v)))
        ((ID (heuristics m)) v)))
    (define (id v) (id~ '_ v))
    (define x default-x-value)
    (define s (make-serializer))
    (define (get-value) x)
    (define (reset) (set! x default-x-value))
    
    (define process-list
      (make-process-list
       (lambda ()
         (id '_)
         ((s (lambda () (id (set! x (* (id x) (id x))))))))
       (lambda ()
         (id '_)
         ((s (lambda () (id (set! x (* (id x) (id x) (id x))))))))))

    (make-process-group heuristics expected-values process-list reset get-value))

  "to see how the values are obtained, SET DEBUG ON in id~"
  "---"
  (parallel-execute (process-test1))
  "---"
  (parallel-execute (process-test3))
  "---"
  (parallel-execute (process-test4))
  "---"
  (parallel-execute (process-test5))
  "---"
  (parallel-execute (process-test6))

  'done
  )

(module+ export
  (provide make-serializer
           parallel-execute
           make-process-group
           make-process
           ID
           make-process-list
           default-heuristics
           make-mutex))

