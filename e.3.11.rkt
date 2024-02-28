#lang racket

;;;
;;;                                                                                    ;artist-mode: call of make-account
;;;  +-------------------------------------------------------------------------------------------------------------------+
;;;  | make-account                                                 acc                 acc2        GLOBAL ENVIRONMENT   |
;;;  |        |                                                      |                   |                               |
;;;  |        |                                                      |                   |                               |
;;;  +--------|------------------------------------------------------+-------------------|-------------------------------+
;;;           |   ^                 ^                                |                   |           ^
;;;           |   |                 |                                |                   |           |
;;;           V   |                 |    ENV-*                       |                   |           |   ENV-**
;;;  +--------+---+---+       +-----+--------+                       |                   |     +-----+--------+
;;;  |   o    |   o   |       | balance: 50  |                       |                   |     | balance: 100 |
;;;  +---+----+-------+       | withdraw: -------+                   |                   |     | withdraw: -------+
;;;      |                    | deposit: --------|---+               |                   |     | deposit: -------+|
;;;      |                    | dispatch: -------|---|---------------+----+              |     | dispatch: -----+||
;;;  o param: balance         +--------------+   |   |               |    |              |     +--------------+ |||
;;;  o code:                  (make-account 50)  |   |               |    |              |    (make-account 100)|||
;;;  (define (withdraw ...))                     |   |               |    |              |                      |||
;;;  (define (deposit ...))     +----------------+   |               |    |              |                      |||
;;;  (define (dispatch ...))    |                    |               |    |              |                      |||
;;;  dispatch                   |   ENV-*            |   ENV-*       |    |   ENV-*      |                      |||
;;;                             |   ^                |   ^           |    |   ^          |                      |||
;;;                             v   |                v   |           V    v   |          |                      |||
;;;                    +--------+---+---+   +--------+---+---+   +--------+---+---+      |                      |||
;;;                    |   o    |   o   |   |   o    |   o   |   |   o    |   o   |      |                      |||
;;;                    +---+----+-------+   +---+----+-------+   +---+----+-------+      +-----------------+    |||
;;;                    o param: amount      o param: amount      o param: m                                |    |||
;;;                    o code:              o code:              o code:                                   |    |||
;;;                    (if (>= bal,, am..)  (set! balance ...)   (cond (eq? m 'withdraw)...)               |    |||
;;;                    (CODE WITHDRAW)      (CODE DEPOSIT)       (CODE DISPATCH)                           |    |||
;;;                                                                                                        |    |||
;;;    *************************************************                                   +---------------|----|+|
;;;    * CALLS via acc: ((acc 'deposit) 40)            *              +--------------------+---------------|----|-+
;;;    * ENV-* <---+                ENV-* <---+        *              |                    |               |    |
;;;    *           |                          |        *              |   ENV-**           |   ENV-**      |    |   ENV-**
;;;    *    +------+------+           +-------+----+   *              |   ^                |   ^           |    |   ^
;;;    *    | m: 'deposit |           | amount: 40 |   *              v   |                v   |           v    |   |
;;;    *    +-------------+           +------------+   *     +--------+---+---+   +--------+---+---+   +---+----+---+---+
;;;    *    (acc 'deposit)            (deposit 40)     *     |   o    |   o   |   |   o    |   o   |   |   o    |   o   |
;;;    * _____________________________________________ *     +---+----+-------+   +---+----+-------+   +---+----+-------+
;;;    *                                               *         |                    |                    |
;;;    * ENV-* <---+                 ENV-* <---+       *     SHARED CODE           SHARED CODE          SHARED CODE
;;;    *           |                           |       *     withdraw              deposit              dispatch
;;;    *    +------+-------+           +-------+----+  *
;;;    *    | m: 'withdraw |           | amount: 40 |  *
;;;    *    +--------------+           +------------+  *
;;;    *    (acc 'withdraw)            (withdraw 40)   *
;;;    *                                               *
;;;    *************************************************
;;;

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

(module+ test
  (define acc (make-account 50))

  "-----------------"

  ((acc 'deposit) 40)
  ((acc 'withdraw) 60)

  (define acc2 (make-account 100)))
