#lang racket

(require "sicp.rkt")
(GETMOD 3 39 without make-mutex)
;; (GETMOD 3 46)
(require scheme/mpair)
;; (require compatibility/mlist)

(define make-semaphore-from-mutex
  (lambda (n)
    "use a mutex to serialize WAIT and SIGNAL. Use THREAD-RECEIVE to
make the thread wait."
    (let ((counter n)
          (waiting-threads '()))
      (define (thread-wait) (thread-receive))
      (define (thread-release t) (thread-send t 'go))
      (define (add-thread-in-queue)
        (set! waiting-threads
              (append waiting-threads
                      (list (current-thread)))))
      (define (remove-first-thread)
        (set! waiting-threads (cdr waiting-threads)))
      (define (wait)
        (if (zero? counter)
            (begin
              (add-thread-in-queue)
              (d "WAITING")
              true)
            (begin (set! counter (sub1 counter))
                   false)))
      (define (signal)
        (if (null? waiting-threads)
            (set! counter (add1 counter))
            (begin (thread-release (car waiting-threads))
                   (remove-first-thread))))
      (let ((s (make-serializer)))
        (let ((s-wait (s wait))
              (s-signal (s signal)))
          (define (acquire) (and (s-wait) (thread-wait)))
          (define (release) (s-signal))
          (define (dispatch m)
            (cond ((eq? m 'P) (acquire) 'done)  ; passeren
                  ((eq? m 'V) (release) 'done)  ; vrijgeven
                  ((eq? m 'counter) counter)
                  (else "Unknown message -- SEMAPHORE")))
          dispatch)))))

(define make-semaphore-from-test-and-set
  (lambda (n)
    "test-and-set! is implemented as S-WAIT to test and S-SIGNAL to
set. They are mutually exclusive, so no other process has effect on
the COUNTER when each of them access it."
    ;; (define (test-and-set! cell)
    ;;   (if (car cell)
    ;;       true
    ;;       (begin (set-mcar! cell true)
    ;;              false)))
    (let ((counter n)
          (cell true))
      (define (wait)
        (if (zero? counter)
            (begin (d "WAITING")
                   true)
            (begin (set! counter (sub1 counter))
                   false)))
      (define (signal)
        (set! counter (add1 counter)))
      (let ((s (make-serializer)))
        (let ((s-wait (s wait))
              (s-signal (s signal)))
          (define (acquire) (and (s-wait) (begin (sleep (/ (random) 100))
                                                 (acquire))))
          (define (release) (s-signal))
          (define (dispatch m)
            (cond ((eq? m 'P) (acquire) 'done) ; passeren
                  ((eq? m 'V) (release) 'done) ; vrijgeven
                  ((eq? m 'counter) counter)
                  (else "Unknown message -- SEMAPHORE")))
          dispatch)))))

(module+ test
  (require racket/sandbox)
  (require rackunit)
  (define (test semaphore-constructor)
    (define s (semaphore-constructor 2))
    (define x1 101 )
    (define x2 102 )
    (define x3 103 )
    (define x4 104 )
    (define x5 105 )
    (define x6 106 )
    (define (display-counter) (d "== Counter" (s 'counter)))
    (display-counter)
    (define (process-test1)
      (define heuristics (lambda (_) (lambda () (if (< (random) .6)
                                                    (/ (random) 100)
                                                    0))))
      (define expected-values -100)
      (define id~ (lambda (m v) ((ID (heuristics m)) v)))
      (define (id v) (id~ '_ v))
      (define (reset)
        (set! x1 101 )
        (set! x2 102 )
        (set! x3 103 )
        (set! x4 104 )
        (set! x5 105 )
        (set! x6 106 )
        (d "---"))
      (define (get-value) (list x1 x2 x3 x4 x5 x6))
      (define (check-sem-counter) (and (> (s 'counter) 2)
                                       (error "counter greater than 2")))
      (define (w k) (check-sem-counter) (d (~a "  > @" k " [" (s 'counter) "]")))
      (define (z k) (check-sem-counter) (d (~a "<   @" k " [" (s 'counter) "]")))
      (define process-list
        (make-process-list
         (lambda () (id '~) (w 1) (s 'P) (id '~) (set! x1 (add1 x1)) (s 'V) (z 1))
         (lambda () (id '~) (w 2) (s 'P) (id '~) (set! x2 (add1 x2)) (s 'V) (z 2))
         (lambda () (id '~) (w 3) (s 'P) (id '~) (set! x3 (add1 x3)) (s 'V) (z 3))
         (lambda () (id '~) (w 4) (s 'P) (id '~) (set! x4 (add1 x4)) (s 'V) (z 4))
         (lambda () (id '~) (w 5) (s 'P) (id '~) (set! x5 (add1 x5)) (s 'V) (z 5))
         (lambda () (id '~) (w 6) (s 'P) (id '~) (set! x6 (add1 x6)) (s 'V) (z 6))))
      (make-process-group heuristics expected-values process-list reset get-value))
    (parallel-execute (process-test1))
    (display-counter))

  "-- MAXIMUM 2 THREADS RUNNING AT EACH MOMENT --"
  "--- SEMAPHORE WITH MUTEX"
  (test make-semaphore-from-mutex)
  "--- SEMAPHORE WITH TEST-AND-SET!"
  (test make-semaphore-from-test-and-set)
  'done)

