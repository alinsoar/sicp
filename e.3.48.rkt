#lang racket

(require "sicp.rkt")
(GETMOD 3 39)

(define heuristics (lambda (_) (lambda () .001 )))
(define id~ (lambda (m v) ((ID (heuristics m)) v)))
(define (id v) (id~ '_ v))

(define (make-account-and-serializer balance ID)
  (define (withdraw amount)
    (d "WITHDRAW" ID balance amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (d "DEPOSIT" ID balance amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            ((eq? m 'get-ID) ID)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

(define (deposit account amount)
  ((account 'deposit) amount))

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    (d (format "EXCHANGE [~a -> ~a] [~a ~a] ~a"
               (account1 'get-ID)  (account2 'get-ID)
               (account1 'balance) (account2 'balance)
               difference))
    ((account1 'withdraw) difference) ;THIS AQUIRE X's mutex again
    ((account2 'deposit) difference)))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    (d (format "## SERIALIZED EXCHANGE [~a ~a] [~a ~a]"
               (account1 'get-ID)  (account2 'get-ID)
               (account1 'balance) (account2 'balance)))
    ((serializer1 (lambda (a b)
                    ;; we insert a delay between acquiring the mutex
                    ;; of the first and the second account, to be sure
                    ;; that we are able to catch quickly the dead-lock
                    ;; given by our conditions.
                    (id '~)
                    ((serializer2 (lambda (m n)
                                    (exchange m n)))
                     a b)))
     account1
     account2)))

(define (serialized-exchange-without-deadlock account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    (d (format "## SERIALIZED EXCHANGE [~a ~a] [~a ~a]"
                (account1 'get-ID)  (account2 'get-ID)
               (account1 'balance) (account2 'balance)))
    ((if (< (account1 'get-ID) (account2 'get-ID))
         (serializer2 (lambda (a b)
                        ;; we keep the same delay, as in the case of
                        ;; the presence of the deadlock
                        (id '~)
                        ((serializer1 (lambda (m n)
                                        (exchange m n)))
                         a b)))
         (serializer1 (lambda (a b)
                        ;; keep the same delay
                        (id '~)
                        ((serializer2 (lambda (m n)
                                        (exchange m n)))
                         a b))))
     account1
     account2)))

(module+ test
  (require racket/sandbox)
  (require rackunit)
  (define (test-serialized-exchange serialized-exchange-procedure)
    (define expected-values -100)
    (define balances '())
    (define x (make-account-and-serializer 10 1))
    (define y (make-account-and-serializer 20 2))
    (define (w a b)
      (d (format "@ exchange process [~a ~a] ~a ~a"
                 (a 'get-ID)  (b 'get-ID)
                 (a 'balance) (b 'balance))))
    (define process-list
      (make-process-list
       (lambda () (w y x) (serialized-exchange-procedure y x))
       (lambda () (w x y) (serialized-exchange-procedure x y))))
    (define (get-value)
      (let ((v (map (lambda (acc) (acc 'balance))
                    (list x y))))
        (cons (~a "sum=" (foldl + 0 v) ";" ) v)))
    (define (reset)
      (set! x (make-account-and-serializer 10 1))
      (set! y (make-account-and-serializer 20 2))
      (d "---"))
    (make-process-group heuristics expected-values process-list reset get-value))

  "--- WITH DEAD-LOCK"
  ;; (parallel-execute (test-serialized-exchange))

  (infinite-loop (lambda () (parallel-execute (test-serialized-exchange
                                          serialized-exchange)))
                 "found DEADLOCK"
                 2
                 "aquire a mutex 2 times")

  "--- WITHOUT DEAD-LOCK"
  (parallel-execute (test-serialized-exchange
                     serialized-exchange-without-deadlock))
  'done )

(module+ export
  (provide id
           heuristics
           exchange
           make-account-and-serializer))
