#lang racket

(require scheme/mpair)
(require compatibility/mlist)

(define (make-queue)
  (let ((front-ptr '()) (rear-ptr '()))
    (define (set-front-ptr! item) (set! front-ptr item))
    (define (set-rear-ptr! item) (set! rear-ptr item))
    (define (empty-queue?) (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT called with an empty queue")
          (mcar front-ptr)))
    (define (insert-queue! item)
      (let ((new-pair (mcons item '())))
        (cond ((empty-queue?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair) )
              (else
               (set-mcdr! rear-ptr new-pair)
               (set-rear-ptr! new-pair))) )
      'ok)
    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! called with an empty queue"))
            (else
             (set-front-ptr! (mcdr front-ptr))))
      'ok)
    (define (print-queue)
      (define (loop x)
        (or (null? x)
            (begin
              (display (mcar x))
              (loop (mcdr x) ) ) ) )
      (newline)
      (display "QUEUE:")
      (loop front-ptr))
    (define (dispatch m)
      (cond ((eq? m 'insert) insert-queue!)
            ((eq? m 'delete) delete-queue!)
            ((eq? m 'front) front-queue)
            ((eq? m 'get-front-ptr) front-ptr)
            ((eq? m 'empty?) empty-queue?)
            ((eq? m 'print) print-queue)
            ((eq? m 'str) str-queue)
            (else "unknown message") ) )
    dispatch ) )

(define insert-queue! (lambda (queue x) ((queue 'insert) x)))
(define delete-queue! (lambda (queue) ((queue 'delete))))
(define empty-queue? (lambda (queue) ((queue 'empty?))))
(define front-queue (lambda (queue) ((queue 'front))))
(define print-queue (lambda (queue) ((queue 'print))))
(define str-queue (lambda (queue) ((queue 'str))))

(module+ test

  (define q1 (make-queue))
  (print-queue q1)

  (insert-queue! q1 'a)
  (print-queue q1)

  (insert-queue! q1 'b)
  (print-queue q1)
  (front-queue q1)

  (delete-queue! q1)
  (print-queue q1)

  (delete-queue! q1)
  (print-queue q1))

(module+ export
  (provide make-queue
           empty-queue?
           front-queue
           delete-queue!
           insert-queue!
           print-queue
           str-queue))
