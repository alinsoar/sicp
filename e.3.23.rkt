#lang racket

(require scheme/mpair)
(require compatibility/mlist)

(define (front-ptr deque) (mcar deque))
(define (rear-ptr deque) (mcdr deque))
(define (set-front-ptr! deque item) (set-mcar! deque item))
(define (set-rear-ptr! deque item) (set-mcdr! deque item))

;;;
;;;    DEQUE:
;;;
;;;       +-------+-------+
;;;       | front | rear  |
;;;       |   o   |   o   |
;;;       +---|---+---|---+
;;;           |       |
;;;        +---       +-----------------------+
;;;        |                                  |
;;;        V                                  V
;;;        +------+------+------+             +------+------+------+
;;;        | item | next | prev |             | item | next | prev |
;;;        |      |   o  |   o  |             |      |   o  |   o  |
;;;        +------+---|--+---|--+             +------+---+--+---|--+
;;;        ^          |      |                ^          |      |
;;;        |          |      |                |          |      |
;;;        |          +------|----------------+          |      |
;;;        +-----------------|---------------------------|------+
;;;                          |                           |
;;;           EMPTY-MLIST <---+                           +--> EMPTY-MLIST
;;;

(define (get-item cell-ptr) (mcar cell-ptr))
(define (get-next cell-ptr) (mcar (mcdr cell-ptr)))
(define (get-prev cell-ptr) (mcar (mcdr (mcdr cell-ptr))))
(define (set-item! cell-ptr item) (set-mcar! cell-ptr item))
(define (set-next! cell-ptr next) (set-mcar! (mcdr cell-ptr) next))
(define (set-prev! cell-ptr prev) (set-mcar! (mcdr (mcdr cell-ptr)) prev))
(define (make-new-cell item) (mlist item '() '()))

(define (empty-deque? deque) (null? (front-ptr deque)))

;;; make-deque:
;;;      +-------+-------+
;;;      | front | rear  |
;;;      |   o   |   o   |
;;;      +---+---+---+---+
;;;          |       |
;;;          V       V
;;;      +---------------+
;;;      |  EMPTY MLIST   |
;;;      +---------------+

(define (make-deque) (mcons '() '()))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (mcar (front-ptr deque))))

(define (front-insert-deque! deque item)
  (let ((new-cell (make-new-cell item)))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-cell)
           (set-rear-ptr! deque new-cell)
           deque)
          (else
           (let ((old-front (front-ptr deque)))
             (set-prev! old-front new-cell)
             (set-next! new-cell old-front) )
           (set-front-ptr! deque new-cell)
           'ok))))

(define (rear-insert-deque! deque item)
  (let ((new-cell (make-new-cell item)))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-cell)
           (set-rear-ptr! deque new-cell)
           deque)
          (else
           (let ((old-rear (rear-ptr deque)))
             (set-next! old-rear new-cell)
             (set-prev! new-cell old-rear) )
           (set-rear-ptr! deque new-cell)
           'ok))))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty deque" deque))
        (else
         (let ((next-front (get-next (front-ptr deque))))
           (if (null? next-front)
               (begin
                 (set-front-ptr! deque '())
                 (set-rear-ptr! deque '() )
                 'ok)
               (begin
                 (set-front-ptr! deque next-front)
                 (set-prev! next-front '() )
                 'ok) ) ) ) ) )

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty deque" deque))
        (else
         (let ((prev-rear (get-prev (rear-ptr deque))))
           (if (null? prev-rear)
               (begin
                 (set-front-ptr! deque '())
                 (set-rear-ptr! deque '() )
                 'ok)
               (begin
                 (set-rear-ptr! deque prev-rear)
                 (set-next! prev-rear '() )
                 'ok) ) ) ) ) )

(define (print-deque deque)
  (define (loop x n limit)
    (cond ((null? x)
           '())
          ((> n limit)
           'break)
          (else
           (display (get-item x))
           (loop (get-next x) (+ 1 n) limit ) ) ) )
  (newline)
  (display "DEQUE:")
  (loop (front-ptr deque) 0 100 )
  (newline))

(module+ test

  (define q1 (make-deque))
  (print-deque q1)

  (rear-insert-deque! q1 'a)
  (print-deque q1)
  
  (rear-insert-deque! q1 'b)
  (print-deque q1)

  (rear-delete-deque! q1)
  (print-deque q1)

  (front-delete-deque! q1)
  (print-deque q1)

  (rear-insert-deque! q1 'x)
  (print-deque q1)

  (rear-delete-deque! q1)
  (print-deque q1)
  )
