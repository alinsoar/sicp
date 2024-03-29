
(load "aux/msim")

;;; to represent in machine the `Y` data structure

;; (define x (cons 1 2))
;; (define y (list x x))

(define internal->lisp
  (lambda (the-cars the-cdrs idx)
    "convert the list at IDX into lisp format."
    ((lambda (s) (s s idx))
     (lambda (s idx)
       (let ((get (lambda (x)
                    (let ((type (car x))
                          (value (cadr x)))
                      (case type
                        ('e '())
                        ('n value)
                        ('s value)
                        ('p (s s value))
                        (else (error "unknown type" x)))))))
         (cons (get (vector-ref the-cars idx))
               (get (vector-ref the-cdrs idx))))))))

(define make-free-list
  (lambda (size return)
    ;; link together all the unused pairs into a free list
    ((lambda (s) (s s 0 (lambda (the-cars the-cdrs)
                     (return (list->vector the-cars)
                             (list->vector the-cdrs)))))
     (lambda (s n col)
       "this is the iota function"
       ;; result: (cons (iota size 0 1) (iota size 1 1))
       (if (>= n size)
           (col '() '())
           (s s (+ n 1)
              (lambda (a d)
                (col (cons n a)
                     (if (= n size)
                         ;; memory full
                         (cons 'end-of-memory d)
                         (cons n d))))))))))

(define machine1
  (make-machine
   '(the-cars the-cdrs val)
   `(
     ;; convert the list at IDX into lisp format.
     (internal->lisp ,internal->lisp)
     
     (make-vector ,(lambda values (apply vector values)))
     (display ,__d))
   '(controller
     (assign the-cars (op make-vector) (const (e 0)) (const (p 5)) (const (n 3)) (const (e 0)) (const (n 4)) (const (n 1)) (const (e 0)) (const (n 2)))
     (perform (op display) (reg the-cars))
     (perform (op display) (reg the-cdrs))
     (assign val (op internal->lisp) (reg the-cars) (reg the-cdrs) (const 1))
     )))

(define machine2
  (make-machine
   '(the-cars the-cdrs val)
   `(
     (internal->lisp ,internal->lisp)
     (make-vector ,(lambda values (apply vector values)))
     (display ,__d))
   '(controller
     (perform (op display) (reg the-cars))
     (perform (op display) (reg the-cdrs))
     (assign val (op internal->lisp) (reg the-cars) (reg the-cdrs) (const 4)))))

(define machine3
  (make-machine
   '(the-cars the-cdrs free k1 k2 x y val-x val-y tmp1 tmp2)
   `((internal->lisp ,internal->lisp)
     (vector-set! ,vector-set!)
     (vector-ref ,vector-ref)
     (display ,__d)
     (+ ,+)
     (mkpointer ,(lambda (v) (list 'p v)))
     (mknumber ,(lambda (v) (list 'n v))))
   '(controller
     ;; in order for internal->lisp to work fine we need to tag the data.
     
     ;; get-new-pair for X = (k1 . k2) then set the car and the cdr of X
     (assign tmp1 (op mknumber) (reg k1))
     (perform (op vector-set!) (reg the-cars) (reg free) (reg tmp1))
     (assign tmp1 (op mknumber) (reg k2))
     (perform (op vector-set!) (reg the-cdrs) (reg free) (reg tmp1))
     (assign x (reg free))
     (assign free (op +) (const 1) (reg free))

     ;; get-new-pair for TMP1 = (X)
     (assign tmp1 (op mkpointer) (reg x))
     (perform (op vector-set!) (reg the-cars) (reg free) (reg tmp1))
     (perform (op vector-set!) (reg the-cdrs) (reg free) (const (e 0)))
     (assign tmp1 (reg free))
     (assign free (op +) (const 1) (reg free))

     ;; get-new-pair for Y = (X X)
     (assign tmp2 (op mkpointer) (reg x))
     (perform (op vector-set!) (reg the-cars) (reg free) (reg tmp2))
     (assign tmp2 (op mkpointer) (reg tmp1))
     (perform (op vector-set!) (reg the-cdrs) (reg free) (reg tmp2))
     (assign y (reg free))
     (assign free (op +) (const 1) (reg free))

     (assign val-x (op internal->lisp) (reg the-cars) (reg the-cdrs) (reg x))
     (assign val-y (op internal->lisp) (reg the-cars) (reg the-cdrs) (reg y))
     
     )))

;;; this is the example from the subchapter 5.3.1
(set-register-contents! machine1 'the-cdrs (vector '(e 0) '(p 2) '(p 4)  '(e 0) '(e 0) '(p 7) '(e 0) '(e 0)))
(start machine1)
(__d (get-register-contents machine1 'val))

;;; this is  y=((1 . 2) 1 . 2) but it is not generated by running CAR CDR and CONS.
(set-register-contents! machine2 'the-cars (vector '_ '(n 1) '_  '_ '(p 1)))
(set-register-contents! machine2 'the-cdrs (vector '_ '(n 2) '_  '_ '(p 1)))
(start machine2)
(__d (get-register-contents machine2 'val))

(make-free-list
 20
 (lambda (the-cars the-cdrs)
   (__d "----")
   (set-register-contents! machine3 'free 0)
   (set-register-contents! machine3 'k1 1)
   (set-register-contents! machine3 'k2 2)
   (set-register-contents! machine3 'the-cars the-cars)
   (set-register-contents! machine3 'the-cdrs the-cdrs)
   (start machine3)
   (__d "x:" (get-register-contents machine3 'x))
   (__d "x:" (get-register-contents machine3 'val-x))
   (__d "y:" (get-register-contents machine3 'y))
   (__d "y:" (get-register-contents machine3 'val-y))
   (__d "a:" (get-register-contents machine3 'the-cars))
   (__d "d:" (get-register-contents machine3 'the-cdrs))
   (__d ":" (get-register-contents machine3 'tmp1))
   (__d ":" (get-register-contents machine3 'tmp2))))


