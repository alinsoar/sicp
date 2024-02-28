#lang racket

(require "sicp.rkt")
(GETMOD 3 39)

(define heuristics
  (lambda (m)
    (lambda () (let ((r (random)))
                 (cond ((< .1 r) 0)
                       ((< .2 r) (/ (random) 5000000))
                       ((< .5 r) (/ (random) 500000))
                       ((< .8 r) (/ (random) 10000))
                       ((< .9 r) (/ (random) 5000))
                       (else (/ (random) 500)))))))

(define id~ (lambda (m v) ((ID (heuristics m)) v)))
(define (id v) (id~ '_ v))

(define (make-account balance)
  (define (withdraw amount)
    (for ((i 5)) (id '_))
    (if (>= (id balance) amount)
        (begin (id '_) (id (set! balance (id (- (id balance) amount))))
               (id '_) balance)
        "Insufficient funds"))
  (define (deposit amount)
    (for ((i 5)) (id '_))
    (id (set! balance (id (+ (id balance) amount))))
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

(define (exchange account1 account2)
  (id '_)
  (let ((difference (id (- (id (account1 'balance))
                           (id (account2 'balance))))))
    (id '_)
    ((account1 'withdraw) difference)
    (id '_)
    ((account2 'deposit) difference)))

(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= (id balance) amount)
        (begin (id '_) (id (set! balance (- (id balance) amount)))
               (id '_) (id balance))
        "Insufficient funds"))
  (define (deposit amount)
    (id '_) (id (set! balance (id (+ balance amount))))
    (id '_) (id balance))
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

;; (define (deposit account amount)
;;   (let ((s (account 'serializer))
;;         (d (account 'deposit)))
;;     ((s d) amount)))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    ((serializer1 (serializer2 exchange))
     account1
     account2)))

(module+ test

  (define (test)
    
    (define expected-values 13)

    (define balances '())
    
    (define x (make-account 10))
    (define y (make-account 20))
    (define z (make-account 30))

    (define process-list
      (make-process-list
       (lambda () (id '_) (exchange (id x) (id y)))
       (lambda () (id '_) (exchange (id x) (id z)))
       (lambda () (id '_) (exchange (id y) (id z)))))

    (define (get-value)
      (let ((v (map (lambda (acc) (acc 'balance)) (list x y z))))
        (cons (~a "sum=" (foldl + 0 v) ";" ) v)))
    
    (define (reset)
      (define (reset account value)
        (let ((b (account 'balance)))
          (if (> b value)
              ((account 'withdraw) (- b value))
              ((account 'deposit) (- value b)))))
      (reset x 10)
      (reset y 20)
      (reset z 30))
    
    (make-process-group heuristics expected-values process-list reset get-value)
    )

  (define (test-serialized)
    
    (define expected-values 3)

    (define balances '())
    
    (define x (make-account-and-serializer 10))
    (define y (make-account-and-serializer 20))
    (define z (make-account-and-serializer 30))

    (define process-list
      (make-process-list (lambda () (id '_) (serialized-exchange (id x) (id y)))
                         (lambda () (id '_) (serialized-exchange (id x) (id z)))
                         (lambda () (id '_) (serialized-exchange (id y) (id z)))))
    (define (get-value)
      (let ((v (map (lambda (acc) (acc 'balance))
                    (list x y z))))
        (cons (~a "sum=" (foldl + 0 v) ";" ) v)))
    
    (define (reset)
      (define (reset account value)
        (let ((b (account 'balance)))
          (if (> b value)
              ((account 'withdraw) (- b value))
              ((account 'deposit) (- value b)))))
      (reset x 10)
      (reset y 20)
      (reset z 30))
    
    (make-process-group heuristics expected-values process-list reset get-value)
    )

  "--- NON SERIALIZED"
  (parallel-execute (test))
  "--- SERIALIZED"
  (parallel-execute (test-serialized))
  
  'done )
