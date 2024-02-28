#lang racket

(require scheme/mpair)
(require compatibility/mlist)

(define (front-ptr queue) (mcar queue))
(define (rear-ptr queue) (mcdr queue))
(define (set-front-ptr! queue item) (set-mcar! queue item))
(define (set-rear-ptr! queue item) (set-mcdr! queue item))
(define (empty-queue? queue) (null? (front-ptr queue)))

;; make-queue:
;;      +-------+-------+
;;      | front | rear  |
;;      |   o   |   o   |   (mcons () ())
;;      +---+---+---+---+
;;          |       |
;;          V       V
;;      +---------------+        | In MIT-scheme () is called
;;      |  EMPTY LIST   |   ()   | ** THE EMPTY LIST **
;;      +---------------+        |
;;                               | (pair?   ()) => #f --  EXCLUSIVE DISJUNCTION
;;                               | (null?   ()) => #t     for null? and pair?
;;                               |
;;                               | (length  ()) => 0
;;                               | (symbol? ()) => #f -- NOT attached a symbol NIL !
;;                               | (list?   ()) => #t
;;                               | (mcar     ()) => ERROR!
;;                               | (mcdr     ()) => ERROR!

(define (make-queue) (mcons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (mcar (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (mcons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-mcdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (mcdr (front-ptr queue)))
         queue)))

(define (print-queue queue)
  (define (loop x co)
    (if (null? x)
        (begin (co (display ":"))
               (newline))
        (loop (cdr x)
              (lambda (_)
                (co '())
                (display " ")
                (display (car x))))))
  (display "QUEUE")
  (loop (mlist->list (front-ptr queue))
        (lambda (_) (display ":")))
  (newline))

(module+ test

  ;; EXERCISE 3.21
  (define q1 (make-queue))
  (print-queue q1)

  ;; +---------------+
  ;; | front | rear  |
  ;; |   o   |   o   |
  ;; +---+---+---+---+
  ;;     |       |        +-------------------+
  ;;     |       |   *----+ mark a POINTER TO |
  ;;     V       V  /     | EMPTY LIST as /   |
  ;; +-------------/-+    +-------------------+
  ;; |   a   |   /   |
  ;; +-------+-------+

  (insert-queue! q1 'a)
  (print-queue q1)

  ;; +---------------+
  ;; | front | rear  |
  ;; |   o   |   o   |
  ;; +---+---+---+---+
  ;;     |       |
  ;;     |       +------------------+
  ;;     V                          V
  ;; +---------------+      +-------+-------+
  ;; |   a   |   o   +----->|   b   |   /   |
  ;; +-------+-------+      +-------+-------+

  (insert-queue! q1 'b)
  (print-queue q1)

  ;; +---------------+
  ;; | front | rear  |
  ;; |   o   |   o   |
  ;; +---+---+---+---+
  ;;     |       +----------------------+
  ;;     +----------------------+       |
  ;;                            |       |
  ;;                            V       V
  ;; +---------------+      +-------+-------+
  ;; |   a   |   o   +----->|   b   |   /   |
  ;; +-------+-------+      +-------+-------+

  (delete-queue! q1)
  (print-queue q1)

  ;; +---------------+
  ;; | front | rear  |
  ;; |   o   |   o   |
  ;; +---+---+---+---+
  ;;     |       +------------------+
  ;;     |                          |
  ;;     +-> EMPTY-LIST             |
  ;;                                V
  ;; +---------------+      +-------+-------+
  ;; |   a   |   o   +----->|   b   |   /   |
  ;; +-------+-------+      +-------+-------+

  (delete-queue! q1)
  (print-queue q1)

  (format "'b is still there: ~a" q1)
  )

(module+ export
  (provide print-queue))


