
(define __d
  (lambda a
    ((lambda (s) (s s a))
     (lambda (s a)
       (if (null? a)
           (newline)
           (begin
             (display (car a))
             (display " ")
             (s s (cdr a))))))))

(define desugar
  (lambda (code)
    (if (not (pair? code))
        code
        (case (car code)
          ((begin)
           ((lambda (s) (s s (cdr code) (lambda (x) x)))
            (lambda (s code col)
              (let ((d (desugar (car code))))
                (if (null? (cdr code))
                    (col d)
                    (s s
                       (cdr code)
                       (lambda (rest)
                         (col `((lambda (_._) ,rest)
                                ,d)))))))))
          ((lambda)
           (case/record2
            code
            (lambda (vars body)
              `(lambda ,vars
                 ,(desugar `(begin . ,(cddr code)))))))
          ((set!)
           (case/record2
            code
            (lambda (id val)
              `(set! ,id ,(desugar val)))))
          ((quote) code)
          ((if)
           (case/record3
            code
            (lambda (test then else)
              `(if ,(desugar test)
                   ,(desugar then)
                   ,(desugar else)))))
          (else
           (map desugar code) ) ) ) ) )

(define case/record1
  (lambda (exp return)
    (record1 (cdr exp) return)))

(define case/record2
  (lambda (exp return)
    (record2 (cdr exp) return)))

(define case/record3
  (lambda (exp return)
    (record3 (cdr exp) return)))

(define record1
  (lambda (exp return)
    (return (car exp))))

(define record2
  (lambda (exp return)
    (return (car exp) (cadr exp))))

(define record3
  (lambda (exp return)
    (return (car exp) (cadr exp) (caddr exp))))

(define record4
  (lambda (exp return)
    (return (car exp) (cadr exp) (caddr exp) (cadddr exp))))

(define get/args1
  (lambda (exp return)
    (return (cadr exp))))

(define get/args2
  (lambda (exp return)
    (return (cadr exp)
            (caddr exp))))

(define get/args3
  (lambda (exp return)
    (return (cadr exp)
            (caddr exp)
            (cadddr exp))))

(define record
  (lambda (a return)
    (case (length a)
      ((2)
       (return (car a) (cadr a)))
      ((3)
       (return (car a) (cadr a) (caddr a)))
      ((4)
       (return (car a) (cadr a) (caddr a) (cadddr a)))
      (else (error "record:" (length a))))))

(define system-environment
  `((+ . ,+)
    (- . ,-)
    (* . ,*)
    (= . ,=)
    (~ . ,display)
    (> . ,>)
    (cons . ,cons)))

(define take
  (lambda (l n)
    ((lambda (s) (s s l n))
     (lambda (s l n)
       (if (or (null? l) (zero? n))
           '()
           (cons (car l)
                 (s s (cdr l) (- n 1))))))))

(define HALT/CHECK
  ;; check whether the program finished via HALT instruction
  (let ((value 'unbound))
    (lambda (msg)
      (case msg
        ('START (set! value #f))
        ('HALT! (set! value #t))
        ('HALTED?
         (or value
             (error "HALT not executed -- " exp)))
        (else
         (error "HALT/CHECK -- " msg))))))

;;; used in 5.40
(define STACK/LENGTH 2000)
(define mk/stack
  (let ((index/stack 0))
    (lambda (size)
      (set! index/stack (+ 1 index/stack))
      (let ((stack/vector (make-vector size))
            (s/ptr 0)
            (f/ptr 0)
            (id index/stack)
            (stat/max/depth 0))
        (__d "New stack #" id "/ size" size)
        ((lambda (s) (s s))
         (lambda (self)
           (lambda (msg . a*)
             (case msg
               ((fp) f/ptr)
               ((sp) s/ptr)
               ((set-fp!) (set! f/ptr (car a*))
                (self self))
               ((set-sp!) (set! s/ptr (car a*))
                (self self))
               ((pop)
                (let ((n (car a*)))
                  (or (>= (- s/ptr n) 0)
                      (error "stack underflow"))
                  (set! s/ptr (- s/ptr n))
                  (self self)))
               ((push)
                (or (< (+ (length a*) s/ptr) STACK/LENGTH)
                    (error "stack overflow"))
                (set! stat/max/depth
                      (max (+ (length a*) s/ptr)
                           stat/max/depth))
                (for-each
                 (lambda (obj)
                   (vector-set! stack/vector s/ptr obj)
                   (set! s/ptr (+ s/ptr 1)))
                 a*)
                (self self))
               ((index)
                (let ((pointer (car a*))
                      (offset (cadr a*)))
                  (if (eq? pointer 'SP)
                      (set! pointer s/ptr)
                      (if (eq? pointer 'FP)
                          (set! pointer f/ptr)
                          (error "bad index reference")))
                  (vector-ref stack/vector (- pointer offset 1))))
               ((index-set!)
                (let ((pointer (car a*))
                      (offset (cadr a*))
                      (val (caddr a*)))
                  (if (eq? pointer 'SP)
                      (set! pointer s/ptr)
                      (if (eq? pointer 'FP)
                          (set! pointer f/ptr)
                          (error "bad index reference")))
                  (vector-set! stack/vector
                               (- (- pointer offset) 1)
                               val)
                  (self self)))
               ((print)
                (__d "stack #" id "sp:" s/ptr "fp:" f/ptr )
                (for-each (lambda (a idx)
                            (if (= idx s/ptr)
                                (display "---> ")
                                (display "   . "))
                            (display idx)
                            (display "  ")
                            (if a (display a))
                            (newline))
                          (vector->list stack/vector)
                          (iota (+ 1 s/ptr)))
                (newline))
               ((save)
                (subvector stack/vector 0 s/ptr))
               ((restore)
                (let ((v (car a*))
                      (sp (vector-length (car a*))))
                  (for-each (lambda (obj idx)
                              (vector-set! stack/vector idx obj))
                            (vector->list v)
                            (iota (+ 1 sp)))
                  (set! s/ptr sp)
                  (self self)))
               ((stat)
                (display "max/depth: ")
                (display stat/max/depth)
                (newline))
               (else (__d "ignored message:" msg)
                     (exit 0))))))))))

(define set-member?
  (lambda (x s)
    (cond ((null? s) false)
          ((eq? x (car s)) true)
          (else (set-member? x (cdr s))))))

(define set-cons
  (lambda (x s)
    (if (set-member? x s)
        s
        (cons x s))))

(define set-union
  (lambda (s1 s2)
    (if (null? s1)
        s2
        (set-union (cdr s1) (set-cons (car s1) s2)))))

(define set-minus
  (lambda (s1 s2)
    (if (null? s1)
        '()
         (if (set-member? (car s1) s2)
             (set-minus (cdr s1) s2)
             (cons (car s1) (set-minus (cdr s1) s2))))))

(define set-intersect
  (lambda (s1 s2)
    (if (null? s1)
        '()
        (if (set-member? (car s1) s2)
            (cons (car s1) (set-intersect (cdr s1) s2))
            (set-intersect (cdr s1) s2)))))


