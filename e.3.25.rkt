#lang racket

(require scheme/mpair)
(require compatibility/mlist)

(define (make-table same-key?)
  (let ((local-table (mlist '*table*)))
    (define (debug msg)
      (and false
           (begin (display msg)
                  (newline) ) ) )
    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key? key (mcar (mcar records))) (mcar records))
            (else (assoc key (mcdr records)))))
    (define (lookup keys-list table)
      (let ((key (mcar keys-list))
            (tail-keys (mcdr keys-list)))
        (let ((next-item (assoc key (mcdr table) ) ) )
          (cond ((not next-item)
                 (debug "; mismatch")
                 #f)
                ((null? tail-keys)
                 (debug "; last key")
                 next-item)
                ((not (mpair? (mcdr next-item)))
                 (debug "; more keys present, final record reached")
                 #f)
                (else
                 (debug "; final record not reached; more keys")
                 (lookup (mcdr keys-list) next-item))))))
    (define (insert! keys-list value table)
      (let ((key (mcar keys-list))
            (tail-keys (mcdr keys-list)))
        ;; next-item is either a record or a subtable
        (let ((next-item (assoc key (mcdr table))))
          (cond ((and (null? tail-keys)
                      next-item)
                 (debug "; no more keys ; found next agenda")
                 (set-mcdr! next-item value)
                 'ok)
                ((and (null? tail-keys)
                      (not next-item))
                 (debug "; no more keys ; not found next agenda")
                 (set-mcdr! table
                           (mcons
                            (mcons key value)
                            (mcdr table)))
                 'ok )
                ((and tail-keys
                      (not next-item))
                 (debug "; more keys ; not found next agenda")
                 (set-mcdr! table
                           (mcons
                            (mcons key '())
                            (mcdr table)))
                 (insert! (mcdr keys-list) value (mcar (mcdr table)) ) )
                (else
                 (debug "; more keys ; found next agenda")
                 (insert! (mcdr keys-list) value next-item))))))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc)
             (lambda (k) (lookup k local-table)))
            ((eq? m 'insert-proc!)
             (lambda (k v) (insert! k v local-table)))
            ((eq? m 'show) (mlist->list local-table))
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table equal?))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(module+ test

  (define t0 (make-table equal?))
  (t0 'show)

  ((t0 'insert-proc!) (list->mlist '(*math* +)) 10)
  (t0 'show)

  ((t0 'insert-proc!) (list->mlist '(*math* -)) 20)
  (t0 'show)

  ((t0 'insert-proc!) (list->mlist '(*al* st)) 10)
  (t0 'show)

  ((t0 'insert-proc!) (list->mlist '(*ao* mcar)) 20)
  (t0 'show)

  ((t0 'insert-proc!) (list->mlist '(*ao* rb)) 20)
  (t0 'show)

  ((t0 'insert-proc!) (list->mlist '(*ao* rb)) 30)
  (t0 'show)

  ((t0 'lookup-proc) (list->mlist '(*ao* test)))
  ((t0 'lookup-proc) (list->mlist '(*ao*)))
  ((t0 'lookup-proc) (list->mlist '(*ao* mcar)))
  ((t0 'lookup-proc) (list->mlist '(test)))

  ((t0 'lookup-proc) (list->mlist '(*ao* mcar test + - * /)))


  ((t0 'insert-proc!) (list->mlist '(*ao*)) 10)
  (t0 'show)

  ((t0 'lookup-proc) (list->mlist '(*ao* test)))
  ((t0 'lookup-proc) (list->mlist '(*ao*)))
  )


