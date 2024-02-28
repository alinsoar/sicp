#lang racket

(require "sicp.rkt")
(GETMOD 3 24 :)
(define lookup :get)
(define insert! :put)
(define make-table (lambda () (:make-table =)))


;;;    SCHEMA OF COMPUTATION FOR MEMOIZE(LAMBDA(N) ...)                       artist-mode
;;;   +--------------------------------------------------------------------------------------------+
;;;   | memo-fib   memoize                             + - * / fib ...         GLOBAL ENVIRONMENT  |
;;;   |    |        /                                                                              |
;;;   |    |       /                                                                               |
;;;   +----|------/--------------------------------------------------------------------------------+
;;;        |     |  ^                ^               ^
;;;        |     |  |                |               |
;;;   +----+     |  |        +----+--+-+             |
;;;   |          |  |     +> | +  |  + |             |
;;;   |          |  |     |  +-+--+----+             |
;;;   |          |  |     |    |                     |
;;;   |          |  |     |    +                     |      In the case that we define memo-fib as
;;;   |          |  |     |   param: n               |      (memoize fib) the difference is so:
;;;   |          V  |     |   code:                  |
;;;   |     +----+--+-+   |   (lambda (n)            |      In the moment when memo-fib starts
;;;   |     | +  |  + |   |     (cond ((= n 0)...))  |      the computation, `previously-computed-result`
;;;   |     +-+--+----+   |                          |      will have non-nil value only if FIB(X)
;;;   |       |           |                          |      was computed before.
;;;   |       +           |                          |
;;;   |    CODE MEMOIZE   |                          |      If it was computed before, the computation
;;;   |    param: f       +---------+                |      returns the result exactly as detailed
;;;   |    (let ((table             |                |      in the situation presented below.
;;;   |     ...)))                  |                |
;;;   |                             |                |      If the value is not in the table,
;;;   |                             |                |      then the computation FIB(X) is done in
;;;   |                             |                |      FIB(X) calls of FIB, and after that the
;;;   |                     +-------+------+         |      result is inserted in the table. For example,
;;;   |         ENV-*       V       |      |         |      FIB(0) will be computed FIB(x) times for each X.
;;;   |        +------------------+ |      |         |
;;;   |        | f: ----------------+      |         |      In the case presented below, the dual version of
;;;   |        +------------------+        |         |      FIB will check the table after each iteration
;;;   |        (memoize                    |         |      towards 0. So FIB(x) will be called only once
;;;   |          (lambda (n) ...))         |         |      for each X.
;;;   |                                    |         |
;;;   |                        +-----------+         |      I am not going to detail the case (memoize fib)
;;;   |         ENV-**         |                     |      because conceptually it is the same as the other
;;;   |        +---------------+---+                 |      case.
;;;   |        | table: TABLE      |                 |
;;;   |        +-------------------+                 |
;;;   |               ^        ^                     |
;;;   |               |        |\                    |
;;;   |        +----+-+--+     | `-------------------|--------+   +-------------+
;;;   +------> |    |    |     |                     |        |   |             |
;;;            +--+-+----+     |                     /        |   V             |
;;;               |            |                    /|   +----+------+          |
;;;               +            |                   / |   | x: 3      |          |
;;;            param: x        |                  /  |   +-----------+          |
;;;            code:           |                 /   |   (memo-fib 3)           |
;;;            (lambda (x)     |                /    |                          |
;;;              (let ((prev...|               /     |                          |
;;;                ))))        |              /      |    +---------------------+-----------+
;;;                            |             /       |    | previously-computed-result: NIL |
;;;                            |            /        |    +---------------------------------+
;;;                            |           /         |                                 ^
;;;                            |          /          |                                 |
;;;                            |          |          |                +----------------+--+
;;;                            |          |          |                | result: f(x)      |
;;;                            |          |          |                +-------------------+
;;;                            |          |          |                 INSERT F(3) in TABLE
;;;                            |          |          |                 => f(3)
;;;                            |          |    +-----+-------+
;;;                            |          |    | n: 3        |
;;;                            |          |    +-------------+
;;;                            |          |    (f 3)
;;;                            |          |    => (memo-fib 2) +
;;;                            |          |       (memo-fib 1)     # MEMO-FIB(1) is in table;
;;;                            |          |                        # previously-computed-result NOT NIL for X=1
;;;                            |\         |
;;;                            | `--------|-------------------+   +-------------+  (MEMO-FIB 2) called only once
;;;                            |          |                   |   |             |
;;;                            |          |                   |   V             |
;;;                            |          |              +----+------+          |
;;;                            |          |              | x: 2      |          |
;;;                            |          |              +-----------+          |
;;;                            |          |\             (memo-fib 2)           |
;;;                            |          | \                                   |
;;;                            |          |  \                                  |
;;;                            |          |   \           +---------------------+-----------+
;;;                            |          |    \          | previously-computed-result: NIL |
;;;                            |          |     \         +---------------------------------+
;;;                            |          |      \                                     ^
;;;                            |          |       \                                    |
;;;                            |          |        \                  +----------------+--+
;;;                            |          |         \                 | result: f(2)      |
;;;                            |          |          \                +-------------------+
;;;                            |          |          |                 INSERT F(2) in TABLE
;;;                            |          |          |                 => f(2)
;;;                            |          |    +-----+-------+
;;;                            |          |    | n: 2        |
;;;                            |          |    +-------------+
;;;                            |          |    (f 2)
;;;                            |          |    => (memo-fib 1) +
;;;                            |          |       (memo-fib 0)     # MEMO-FIB(0) is in table;
;;;                            |          |                        # previously-computed-result NOT NIL
;;;                            |\         |
;;;                            | `--------|-------------------+   +-------------+  (MEMO-FIB 1) called only once
;;;                            |          |                   |   |             |
;;;                            |          |                   |   V             |
;;;                            |          |              +----+------+          |
;;;                            |          |              | x: 1      |          |
;;;                            |          |              +-----------+          |
;;;                            |          |\             (memo-fib 1)           |
;;;                            |          | \                                   |
;;;                            |          |  \                                  |
;;;                            |          |   \           +---------------------+-----------+
;;;                            |          |    \          | previously-computed-result: NIL |
;;;                            |          |     \         +---------------------------------+
;;;                            |          |      \                                     ^
;;;                            |          |       \                                    |
;;;                            |          |        \                  +----------------+--+
;;;                            |          |         \                 | result: f(1)      |
;;;                            |          |          \                +-------------------+
;;;                            |          |          |                 INSERT F(1) in TABLE
;;;                            |          |          |                 => f(1)
;;;                            |          |    +-----+-------+
;;;                            |          |    | n: 1        |
;;;                            |          |    +-------------+
;;;                            |          |    (f 1)
;;;                            |          |    => 1
;;;                            |          |
;;;                            |          |
;;;                            +----------|-------------------+   +-------------+  (MEMO-FIB 0) called only once
;;;                                       |                   |   |             |
;;;                                       |                   |   V             |
;;;                                       |              +----+------+          |
;;;                                       |              | x: 0      |          |
;;;                                       |              +-----------+          |
;;;                                        \             (memo-fib 0)           |
;;;                                         \                                   |
;;;                                          \                                  |
;;;                                           \           +---------------------+-----------+
;;;                                            \          | previously-computed-result: NIL |
;;;                                             \         +---------------------------------+
;;;                                              \                                     ^
;;;                                               \                                    |
;;;                                                \                  +----------------+--+
;;;                                                 \                 | result: f(0)      |
;;;                                                  \                +-------------------+
;;;                                                  |                 INSERT F(0) in TABLE
;;;                                                  |                 => f(0)
;;;                                            +-----+-------+
;;;                                            | n: 0        |
;;;                                            +-------------+
;;;                                            (f 0)
;;;                                            => 0
;;;


(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))

(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))


(module+ test
  (memo-fib 3)
  )
