#lang racket

(require "sicp.rkt")
(GETMOD 3 39)

(define heuristics
  (lambda (m)
    (lambda ()
      (cond
       ((eq? m 'balance) (/ (random) 10000))
       ((eq? m 'balance-protected) (/ (random) 10000))
       (else (let ((r (random)))
               (cond ((< .2 r) 0)
                     ((< .3 r) (/ (random) 1000000))
                     ((< .4 r) (/ (random) 2000000))
                     ((< .5 r) (/ (random) 5000000))
                     ((< .8 r) (/ (random) 10000000))
                     (else (/ (random) 300000)))))))))

(define id~ (lambda (m v) ((ID (heuristics m)) v)))
(define (id v) (id~ '_ v))

(define (make-account balance)
  (define (withdraw amount)
    (id '_)
    (if (>= (id balance) amount)
        (begin (id '_) (id (set! balance (- (id balance) amount)))
               (id '_) balance)
        "Insufficient funds"))
  (define (deposit amount)
    (id '_)
    (id (set! balance (id (+ balance amount))))
    (id balance))
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (protected withdraw))
            ((eq? m 'deposit)  (protected deposit))
            ((eq? m 'balance-protected) ((protected (lambda () balance))))
            ((eq? m 'balance) balance)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

(module+ test

  (define default-balance 100)
  
  (define (test bal)
    
    (define expected-values -10000)

    (define balances '())
    
    (define x (make-account default-balance))

    (define process-list
      (make-process-list
       (lambda () ((x 'withdraw) 40))
       (lambda () ((x 'deposit) 50))
       (lambda () ((x 'withdraw) 20))
       (lambda ()
         (for ((_ 10)) (and (< (random) .2) (id~ bal '_)))
         (let ((b (x bal)))
           (if (member b balances)
               'already-there
               (begin
                 (set! balances (cons b balances))
                 (d (~a "# " (length balances) " " bal ': b))))))))
    
    (define (get-value) (x bal))
    
    (define (reset)
      (let ((b (x bal)))
        (if (> b default-balance)
            ((x 'withdraw) (- b default-balance))
            ((x 'deposit) (- default-balance b)))))
    
    (make-process-group heuristics expected-values process-list reset get-value)
    )
  
  "--- PROTECTED INTERROGATION OF BALANCE"
  (parallel-execute (test 'balance-protected))
  "--- NON PROTECTED INTERROGATION OF BALANCE"
  (parallel-execute (test 'balance))
  "the results are the same -- 8 found balance values in both cases"
  'done
  )
