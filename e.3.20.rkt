#lang racket

(module c1 racket

  (define (cons x y)
    (define (dispatch m)
      (cond ((eq? m 'car) x)
            ((eq? m 'cdr) y)
            (else (error "Undefined operation -- CONS" m))))
    dispatch)

  (define (car z) (z 'car))
  (define (cdr z) (z 'cdr))

  (provide cons car cdr))

(module c3 racket
  (define (cons x y) (lambda (m) (m
                                  ;; provide 3 permissions and 2 selectors to m
                                  x
                                  y
                                  (lambda (n) (set! x n))
                                  (lambda (n) (set! y n))
                                  (lambda () (format "(~a:~a)" x y)))))
  (define (car x) (x (lambda (a d sa sd p) a)))
  (define (cdr x) (x (lambda (a d sa sd p) d)))
  (define (set-car! x y) (x (lambda (a d sa sd p) (sa y))))
  (define (set-cdr! x y) (x (lambda (a d sa sd p) (sd y))))
  (define (print x) (x (lambda (a d sa sd p) (p))))

  (provide cons car cdr set-car! set-cdr! print))

(module c2 racket
  (define (cons x y)
    (define (set-x! v) (set! x v))
    (define (set-y! v) (set! y v))
    (define (dispatch m)
      (cond ((eq? m 'car) x)
            ((eq? m 'cdr) y)
            ((eq? m 'set-car!) set-x!)
            ((eq? m 'set-cdr!) set-y!)
            (else (error "Undefined operation -- CONS" m))))
    dispatch)
  (define (car z) (z 'car))
  (define (cdr z) (z 'cdr))
  (define (set-car! z new-value)
    ((z 'set-car!) new-value)
    'ok)
  (define (set-cdr! z new-value)
    ((z 'set-cdr!) new-value)
    'ok)

  (provide cons car cdr set-car! set-cdr!))

;;; artist mode: visualizing the 2nd implementation of constructor and selectors
;;; +-------------------------------------------------------------------------------------+
;;; |                                                            GLOBAL ENVIRONMENT       |
;;; |     cons    set-car!    cdr x                            z                    set!  |
;;; |     |           |        |  |                            |               primitive  |
;;; +-----|-----------|--------|--|----------------------------|--------------------------+
;;;       |      ^    |  ^  ^  |  |          ^                 |          ^
;;;       |      |    |  |  |  |  |          |        ENV-*    |          |        ENV-**
;;;       |      |    |  |  |  |  |          |       /         |          |       /
;;;       |      |    |  |  |  |  |  +-------+------+          |  +-------+------+
;;;       V      |    |  |  |  |  |  | x:1          |<------+  |  | x:global.x   |<------+
;;;   +------+---+--+ |  |  |  |  |  | y:2          |       |  |  | y:global.x   |       |
;;;   |      |      | |  |  |  |  |  | dispatch: -----+     |  |  | dispatch: -----+     |
;;;   +------+------+ |  |  |  |  |  | set-x!:--------|-+   |  |  | set-x!:--------|-+   |
;;;   o param: x y    |  |  |  |  |  | set-y!:--------|-|-+ |  |  | set-y!:--------|-|-+ |
;;;   o code: CONS    |  |  |  |  |  +--------------+ | | | |  |  +--------------+ | | | |
;;;                   |  |  |  |  |   x=(cons 1 2)    | | | |  |  z=(cons x x)     | | | |
;;;      +------------+  |  |  |  |                   | | | |  |                   | | | |
;;;      |               |  |  |  |                   | | | |  |                   | | | |
;;;      |               |  |  |  |           +-------+ | | |  |           +-------+ | | |
;;;      |               |  |  |  |           V         | | |  |           V         | | |
;;;      V               |  |  |  |    +------+------+  | | |  |    +------+------+  | | |
;;;   +------+---+--+    |  |  |  +--->|      |  o   +------+  +--->|      |  o   +------+
;;;   |      |   o  +----+  |  |       +------+------+  | | ^       +------+------+  | | ^
;;;   +------+------+       |  |       o param: m       | | |       o param: m       | | |
;;;   o param: z new-value  |  |       o code:          | | |       o code:          | | |
;;;   o code: SET-CAR!      |  |       CONS.DISPATCH    | | |       CONS.DISPATCH    | | |
;;;                         |  |                        | | |                        | | |
;;;                         |  |                        | | |                        | | |
;;;      +------------------|--+              +---------+ | |              +---------+ | |
;;;      |                  |                 V           | |              V           | |
;;;      |                  |          +------+--+---+    | |       +------+--+---+    | |
;;;      V                  |          |      |  o   +------+       |      |  o   +------+
;;;   +------+------+       |          +------+------+    | ^       +------+------+    | ^
;;;   |      |   o  +-------+          o param: v         | |       o param: v         | |
;;;   +------+------+                  o code:            | |       o code:            | |
;;;   o param: z                       CONS.SET-X!        | |       CONS.SET-X!        | |
;;;   o code: CDR                                         | |       (shared)           | |
;;;                                                       | |                          | |
;;;                                           +-----------+ |              +-----------+ |
;;;                                           V             |              V             |
;;;                                    +------+--+---+      |       +------+--+---+      |
;;;                                    |      |  o   +------+       |      |  o   +------+
;;;                                    +------+------+              +------+------+
;;;                                    o param: v                   o param: v
;;;                                    o code:                      o code:
;;;                                    CONS.SET-Y!                  CONS.SET-Y!
;;;                                                                 (shared)
;;;
;;;
;;;           GLOBAL             GLOBAL                  ENV-**               ENV-**
;;;             ^                  ^                       ^                    ^
;;;             |                  |                       |                    |
;;;   +---------+---------+    +---+------------+    +-----+--------+    +------+--------+
;;;   | z: <= (cdr z)     |    | z: <= global.x |    | m: 'cdr      |    | m: 'cdr       |
;;;   | new-value: 17     |    +----------------+    +--------------+    +---------------+
;;;   +-------------------+    (cdr z)               (z 'cdr)            CONS.DISPATCH
;;;   (set-car (cdr z) 17)     => global.x           => global.x         => y from ENV-**
;;;
;;;
;;;             ENV-**                    ENV-*                       ENV-*
;;;               ^                        ^                           ^
;;;               |                        |                           |
;;;    +----------+---------+     +--------+-----------+     +---------+-----------+
;;;    | m: 'set-car!       |     | v: 17              |     | v:17                |
;;;    +--------------------+     +--------------------+     +---------------------+
;;;    (X 'set-car!)              (X.CONS.SET-X! 17)         (set! x v)
;;;    => CONS.SET-X! from ENV-**                                 \
;;;                                                                \
;;;                                                                 \
;;;           GLOBAL                        ENV-*            MUTATES x to 17 in ENV-*
;;;             ^                            ^
;;;             |                            |
;;;    +--------+----------+       +---------+----------+
;;;    | z: X              |       | m: 'car            |
;;;    +-------------------+       +--------------------+
;;;    (car x)                     (X 'car)
;;;                                => 17
;;;

(require (prefix-in c1: 'c1))
(require (prefix-in c2: 'c2))
(require (prefix-in c3: 'c3))

(module+ test
  (define t1 (c1:cons 1 2))
  ;;
  (define x (c2:cons 'a 'b))
  (define z (c2:cons x x))
  ;;
  (define m (c3:cons 'a 'b))
  (define n (c3:cons m m))
  "---"
  t1
  (c1:car t1)
  (c1:cdr t1)
  "---"
  z
  (c2:car z)
  (c2:cdr z)
  "---"
  (c2:car (c2:car z))
  (c2:cdr (c2:car z))
  (c2:car (c2:cdr z))
  (c2:cdr (c2:cdr z))
  "---"
  (c2:set-car! x 'w)
  (c2:car (c2:car z))
  (c2:cdr (c2:car z))
  (c2:car (c2:cdr z))
  (c2:cdr (c2:cdr z))
  "---"
  m
  n
  (c3:set-car! m 'w)
  (c3:car (c3:car n))
  (c3:cdr (c3:car n))
  (c3:car (c3:cdr n))
  (c3:cdr (c3:cdr n))
  (c3:print m)
  (c3:print n)
  (c3:print (c3:car n))
  (c3:print (c3:cdr n))
  )

