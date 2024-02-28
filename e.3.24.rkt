#lang racket

(require scheme/mpair)
(require compatibility/mlist)

(define (make-table same-key?)
  (let ((local-table (mlist '*table*)))
    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key? key (mcar (mcar records))) (mcar records))
            (else (assoc key (mcdr records)))))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (mcdr subtable))))
              (if record
                  (mcdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (mcdr subtable))))
              (if record
                  (set-mcdr! record value)
                  (set-mcdr! subtable
                            (mcons (mcons key-2 value)
                                  (mcdr subtable)))))
            (set-mcdr! local-table
                      (mcons (mlist key-1
                                  (mcons key-2 value))
                            (mcdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'show) local-table)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch) )

(define operation-table (make-table equal?))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(module+ test
  (define t0 (make-table equal?))
  (t0 'show)
  ((t0 'insert-proc!) '*mat* '+ 10)
  ((t0 'insert-proc!) '*alg* 'st 10)
  ((t0 'insert-proc!) '*alg* 'mc 20)
  ((t0 'insert-proc!) '*alg* 'rb 20)
  ((t0 'insert-proc!) '*alg* 'rb 30)
  (t0 'show))

(module+ export
  (provide make-table
           get
           put))
