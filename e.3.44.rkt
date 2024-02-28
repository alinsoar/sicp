#lang racket

(require "sicp.rkt")
(GETMOD 3 39)

(define heuristics
  (lambda (m)
    (lambda ()
      (/ (random) 100000))))

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

(define (transfer from-account to-account amount)
  (id '_) ((from-account 'withdraw) amount)
  (id '_) ((to-account 'deposit) amount))

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

(define (serialized-transfer account1 account2 amount)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    ((serializer1 (serializer2 transfer))
     account1
     account2
     amount)))

(module+ test

  (define (test1)
    
    (define expected-values -100)
    (define balances '())
    
    (define x (make-account 100))
    (define y (make-account 200))
    (define z (make-account 300))

    (define process-list
      (make-process-list
       (lambda () (id '_) (transfer (id y) (id x) 5))
       (lambda () (id '_) (transfer (id y) (id x) 5))
       (lambda () (id '_) (transfer (id y) (id x) 5))
       (lambda () (id '_) (transfer (id z) (id x) 5))
       (lambda () (id '_) (transfer (id z) (id x) 5))
       (lambda () (id '_) (transfer (id z) (id x) 5))))
    (define (get-value)
      (let ((v (map (lambda (acc) (acc 'balance)) (list x y z))))
        (cons (~a "sum=" (foldl + 0 v) ";" ) v)))
    (define (reset)
      (define (reset account value)
        (let ((b (account 'balance)))
          (if (> b value)
              ((account 'withdraw) (- b value))
              ((account 'deposit) (- value b)))))
      (reset x 100)
      (reset y 200)
      (reset z 300))
    
    (make-process-group heuristics expected-values process-list reset get-value)
    )

  (define (test2)
    
    (define expected-values -1000)
    (define balances '())
    
    (define x (make-account 100))
    (define y (make-account 200))
    (define z (make-account 300))

    (define process-list
      (make-process-list
       (lambda () (id '_) (transfer (id x) (id y) 5))
       (lambda () (id '_) (transfer (id x) (id y) 5))
       (lambda () (id '_) (transfer (id x) (id y) 5))
       (lambda () (id '_) (transfer (id x) (id z) 5))
       (lambda () (id '_) (transfer (id x) (id z) 5))
       (lambda () (id '_) (transfer (id x) (id z) 5))))
    (define (get-value)
      (let ((v (map (lambda (acc) (acc 'balance)) (list x y z))))
        (cons (~a "sum=" (foldl + 0 v) ";" ) v)))
    (define (reset)
      (define (reset account value)
        (let ((b (account 'balance)))
          (if (> b value)
              ((account 'withdraw) (- b value))
              ((account 'deposit) (- value b)))))
      (reset x 100)
      (reset y 200)
      (reset z 300))
    
    (make-process-group heuristics expected-values process-list reset get-value)
    )

  (define (test-serialized)
    
    (define expected-values -1000)
    (define balances '())
    
    (define x (make-account-and-serializer 100))
    (define y (make-account-and-serializer 200))
    (define z (make-account-and-serializer 300))

    (define process-list
      (make-process-list (lambda () (id '_) (serialized-transfer (id x) (id y) 5))
                         (lambda () (id '_) (serialized-transfer (id y) (id z) 5))
                         (lambda () (id '_) (serialized-transfer (id x) (id z) 5))
                         (lambda () (id '_) (serialized-transfer (id x) (id y) 5))
                         (lambda () (id '_) (serialized-transfer (id y) (id z) 5))
                         (lambda () (id '_) (serialized-transfer (id x) (id z) 5))))
    (define (get-value)
      (let ((v (map (lambda (acc) (acc 'balance)) (list x y z))))
        (cons (~a "sum=" (foldl + 0 v) ";" ) v)))
    (define (reset)
      (define (reset account value)
        (let ((b (account 'balance)))
          (if (> b value)
              ((account 'withdraw) (- b value))
              ((account 'deposit) (- value b)))))
      (reset x 100)
      (reset y 200)
      (reset z 300))
    
    (make-process-group heuristics expected-values process-list reset get-value)
    )

  "--- TRANSFER TO X"
  (parallel-execute (test1))
  "--- TRANSFER FROM X"
  (parallel-execute (test2))
  "--- SERIALIZED IT'S THE SAME"
  (parallel-execute (test-serialized))
  "Everything is the same, as the deposit/withdraw are protected in each account."
  'done )
