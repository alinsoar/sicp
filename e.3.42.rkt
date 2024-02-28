#lang racket

(require "sicp.rkt")
(GETMOD 3 39)

(define heuristics default-heuristics)

(define id~ (lambda (m v) ((ID (heuristics m)) v)))
(define (id v) (id~ '_ v))

(define (make-account balance)
  (define (withdraw amount)
    (if (>= (id balance) amount)
        (begin (id '_) (id (set! balance (- (id balance) amount)))
               (id '_) (id balance))
        "Insufficient funds"))
  (define (deposit amount)
    (id '_) (id (set! balance (id (+ balance amount))))
    (id '_) (id balance))
  (let ((protected (make-serializer)))
    (let ((protected-withdraw (protected withdraw))
	  (protected-deposit (protected deposit)))
      (define (dispatch m)
	(cond ((eq? m 'withdraw) protected-withdraw)
	      ((eq? m 'deposit) protected-deposit)
	      ((eq? m 'balance) (id '_) balance)
	      (else (error "Unknown request -- MAKE-ACCOUNT"
			   m))))
      dispatch)))

(module+ test

  (define default-balance 100)

  (define (test)
    
    (define expected-values -10000)

    (define balances '())
    
    (define x (make-account default-balance))

    (define process-list
      (make-process-list
       (lambda () ((x 'withdraw) 40))
       (lambda () ((x 'deposit) 50))
       (lambda () ((x 'withdraw) 20))
       (lambda ()
         (for ((_ 20)) (and (< (random) .2) (id '_)))
         (let ((b (x 'balance)))
           (if (member b balances)
               'already-there
               (begin
                 (set! balances (cons b balances))
                 (d (~a "# " (length balances) " " 'balance ': b))))))))

    ;; The single final output value is the same (90) in all
    ;; simulations, so it is correct to protect withdraw and deposit
    ;; only once.
      (define (get-value) (x 'balance))
    
    (define (reset)
      (let ((b (x 'balance)))
        (if (> b default-balance)
            ((x 'withdraw) (- b default-balance))
            ((x 'deposit) (- default-balance b)))))
    
    (make-process-group heuristics expected-values process-list reset get-value)
    )

  "---"
  (parallel-execute (test))
  
  'done )
