
(define executor?)
(define get-executor-metadata)
(define set-executor-metadata!)
(let ((association (make-metadata-association)))
  (set! executor? (association 'has?))
  (set! get-executor-metadata (association 'get))
  (set! set-executor-metadata! (association 'put!)))
(register-predicate! executor? 'executor)

(define (executors? object)
  (and (list? object)
       (every executor? object)))
(register-predicate! executors? 'executors)

(define (make-executor implementation #!optional arg-checker)
  (letrec ((executor
            (lambda args
              (if (not (default-object? arg-checker))
                  (apply arg-checker args))
              (execution-trace 'save! (cons executor args))
              (apply implementation args))))
    (set-executor-metadata! executor implementation)
    executor))

(define (make-circular-buffer size)
  (let ((buffer (make-list size #f)))

    (define (save! item)
      (set-car! buffer item)
      (set! buffer (cdr buffer)))

    (define (get-all)
      (let loop ((b (cdr buffer)) (items (list (car buffer))))
        (if (eq? b buffer)
            items
            (loop (cdr b)
                  (cons (car b) items)))))

    (set-cdr! (last-pair buffer) buffer)
    (bundle circular-buffer? save! get-all)))

(define circular-buffer?
  (make-bundle-predicate 'circular-buffer))

(define (get-execution-trace)
  (execution-trace 'get-all))

(define execution-trace
  (make-circular-buffer 10))