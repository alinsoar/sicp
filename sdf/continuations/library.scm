
;;; The following must be in the AmbScheme evaluator environment

(define (one-of lst)
  (if (null? lst)
      (amb)
      (amb (car lst)
           (one-of (cdr lst)))))

(define (distinct l)
  (cond ((null? l) true)
	((null? (cdr l)) true)
	((member (car l) (cdr l)) false)
	(else (distinct (cdr l)))))

(define (require p)
  (if (not p) (amb)))

(define (map f l)
  (if (null? l)
      '()
      (cons (f (car l))
	    (map f (cdr l)))))

(define (to-right n)
  (modulo (+ n 1) 6))

(define (better-hand? position1 position2 values)
  (> (cdr (assv position1 values))
     (cdr (assv position2 values))))

(define (except set exceptions)
  (one-of (lset-difference eqv? set exceptions)))

(define (assignments places values)
  (if (null? places)
      '()
      (let ((choice (one-of values)))
	(cons (cons (car places) choice)
	      (assignments (cdr places)
			   (delv choice values))))))


;;; For call/cc stuff

(define (call/cc receiver)
  (call/ccs (lambda (succeed fail)
              (receiver (lambda (value)
                          (succeed value fail))))))

(define call-with-current-continuation call/cc)

(define (for-each proc lst)
  (cond ((null? lst) 'done)
        (else (proc (car lst))
              (for-each proc (cdr lst)))))
