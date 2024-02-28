#lang racket

"
                                 ;artist-mode: call of make-withdraw VERSION 1
+---------------------------------------------------------------------------------+
| make-withdraw      W1                                W2    GLOBAL ENVIRONMENT   |
|        |           |                                 |                          |
|        |           |                                 |                          |
+--------|-----------|---------------------------------|--------------------------+
         |   ^       |           ^                     |           ^
         |   |       |           |                     |           |
         |   |       |           |                     |           |
         |   |       |   +-------+-----+  bal-=50      |   +-------+------+
         V   |       |   | balance: 50 | <-------+     |   | balance: 100 | <--------+
+--------+---+---+   |   +-------------+         |     |   +--------------+          |
|   o    |   o   |   |           ^               |     |           ^                 |
+---+----+-------+   |           |               |     |           |                 |
    |                |    +------+------+        |     |    +------+------+          |
 o param: balance    +--->|  o   |   o  |        |     +--->|  o   |   o  |          |
 o code:                  +--+---+------+        |          +--+---+------+          |
  (lambda (amount)           |                   |             |                     |
     ...)                    |                   |             |                     |
                             o param: amount     |             o param: amount       |
                       +---# o code:             |             o code:               |
                       |       (if (>= bal...)   |               (if (>= bal...)     |
                       |          ...)           |                  ...)             |
                       |                         |                                   |
                       |                  +------+-----+                      +------+--+--+
                       |                  | amount: 50 |                      | amount: 50 |
                       |                  +------+-----+                      +------+-----+
                       +----------------#(W1 50)                  shared---#(W2 50)
                       |                 => (if (>= b..)..)       code      => (> (if (>= b..)..)
                       |                                            |
                       +--------------------------------------------+
"

;;;SECTION 3.1.1
(define (make-withdraw1 balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

"                                         ;artist-mode: call of make-withdraw VERSION 2
+------------------------------------------------------------------------------------------+
| make-withdraw          W1                                      W2     GLOBAL ENVIRONMENT |
|        |               |                                       |                         |
|        |               |                                       |                         |
+--------|---------------|---------------------------------------|-------------------------+
         |   ^           |           ^                           |           ^         
         |   |           |           |      +-------+            |           |      +-------+
         |   |           |  ENV*     |      V       |            |           |      V       |
         |   |           | +---------+----------+   |            | +---------+----------+   |
         V   |           | |initial-amount: 100 |   |            | |initial-amount: 100 |   |
+--------+---+---+       | +--------------------+   |            | +--------------------+   |
|   o    |   o   |       | (make-withdraw 100)      |            | (make-withdraw 100)      |
+---+----+-------+       |                          |            |                          |
    |                    |           +--------------+            |           +--------------+
    |                    | +---------+------------+              | +---------+------------+
o param: initial-amount  | |balance:initial-amount  <---+ <----+ | |balance:initial-amount  <---+
o code:                  | +----------------------+     |      | | +----------------------+     |
 (let ((balance..))      | (let (balance ...) ... )     |      | | (let (balance ...) ... )     |
   ...)                  |     /                        |      | |                              |
                         |    /                         |      | |                              |
                         |   /             +--------+---+---+  | |                 +--------+---+---+
                         +--*------------> |   o    |   o   |  | +---------------> |   o    |   o   |
                           /               +---+----+-------+  |                   +---+----+-------+
                          /                    |               |                       |
                         /                 o params: amount    |                   o params: amount
______________________________________     o code:             |                   o code:
: DETAILS of initializing `balance`  :      (if (>= bal...)    |                    (if (>= bal...)
:                                    :         ...)            |                       ...)
:  W1             ENV*               :       #                 |
:(global)          ^       +------+  :       |                 |
:   |              |       |      |  :       |                 |
:   |              |       V      |  :       |                 |
:   |   +----------+-----------+  |  :       |                 |
:   |   |balance:initial-amount|  |  :       |           +-----+-----+
:   |   +----------+-----------+  |  :       |           | amount:50 |
:   |   ((lambda (balance)        |  :       |           +-----------+
:   |       (lambda (amount)      |  :       |            (W1 50)
:   |         ....)               |  :       +----------# (if (>= balance amount)
:   |    initial-amount)          |  :                       ...)
:   |                             |  :
:   |                             |  :
:   +--------------+   +----------+  :
:                  |   |             :
:                  V   |             :
:           +------+---+--+          :
:           |  o   |   o  |          :
:           +--+---+------+          :
:           o params: amount         :
:           o code:                  :
:             (if (>= balance amount):
:                ...)                :
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"


;; EXERCISE 3.10
(define (make-withdraw2 initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

(module+ test
  "---"
  (define W11 (make-withdraw1 50))
  (define W12 (make-withdraw1 100))
  (W11 50)
  (W12 5)

  "---"
  (define W21 (make-withdraw2 100))
  (define W22 (make-withdraw2 100))
  (W21 50)
  )
