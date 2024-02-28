#lang racket

(require "sicp.rkt")
(GETMOD 4 35)

(module+ test
  "===================="
  (amb-test
   '((define (member? a l)
       (define (iter l)
         (cond ((null? l) false)
               ((equal? a (car l)) true)
               (else
                (iter (cdr l)))))
       (iter l))
     (define (map f l)
       (if (null? l)
           '()
           (cons (f (car l))
                 (map f (cdr l)))))
     (define (append l1 l2)
       (if (null? l1)
           l2
           (cons (car l1)
                 (append (cdr l1) l2))))
     (define (distinct? l)
       (define (iter l)
         (cond ((null? l) true)
               ((member? (car l) (cdr l)) false)
               (else
                (iter (cdr l)))))
       (iter l))
     (define counter 0)
     (define (multiple-dwelling)
       (let ((baker (amb 1 2 3 4 5))
             (cooper (amb 1 2 3 4 5))
             (fletcher (amb 1 2 3 4 5))
             (miller (amb 1 2 3 4 5))
             (smith (amb 1 2 3 4 5)))
         (p-set! counter (+ 1 counter))
         (require (distinct? (list baker cooper fletcher miller smith)))
         (require (not (= baker 5)))
         (require (not (= cooper 1)))
         (require (not (= fletcher 5)))
         (require (not (= fletcher 1)))
         (require (> miller cooper))
         (require (not (= (abs (- smith fletcher)) 1)))
         (require (not (= (abs (- fletcher cooper)) 1)))
         (list (list 'baker baker)
               (list 'cooper cooper)
               (list 'fletcher fletcher)
               (list 'miller miller)
               (list 'smith smith)
               counter)))
     (define (multiple-dwelling/scheme)
       
       (define levels '(1 2 3 4 5))

       (define (filter1 p l)
         (define (iter l)
           (cond ((null? l) '())
                 ((p (car l))
                  (cons (car l)
                        (iter (cdr l))))
                 (else (iter (cdr l)))))
         (iter l))

       (define (X p l1 l2)
         "cartesian product with filtering using a predicate"
         (define (iter l1 l2)
           (cond ((null? l1) '())
                 ((null? l2) '())
                 ((p (car l1) (car l2))
                  (cons (cons (car l1) (car l2))
                        (append (iter (list (car l1)) (cdr l2))
                                (append (iter (cdr l1) (list (car l2)))
                                        (iter (cdr l1) (cdr l2))))))
                 (else
                  (append (iter (list (car l1)) (cdr l2))
                          (append (iter (cdr l1) (list (car l2)))
                                  (iter (cdr l1) (cdr l2)))))))
         (iter l1 l2))

       (define baker1
         (lambda (baker)
           (filter1 (lambda (b) (not (= b 5))) baker)))
       (define cooper1
         (lambda (cooper)
           (filter1 (lambda (c) (not (= c 1))) cooper)
           ))
       (define fletcher1
         (lambda (fletcher)
           (filter1 (lambda (f) (if (not (= f 1))
                               (not (= f 5))
                               false))
                    fletcher)))
       (define miller1
         (lambda (l) l))
       (define smith1
         (lambda (l) l))

       (define smfbc
        (filter1
         distinct?
         (X                             ; smith x miller x fletcher x baker x cooper
          (lambda (smith mfbc)
            (define fletcher (car (cdr mfbc)))
            (not (= (abs (- smith fletcher)) 1)))
          (smith1 levels)
          (X                            ; miller x fletcher x baker x cooper
           (lambda (miller fbc)
             (define cooper (car (cdr (cdr fbc))))
             (> miller cooper))
           (miller1 levels)
           (X                           ; fletcher x baker x cooper
            (lambda (fletcher bc)
              (define cooper (car (cdr bc)))
              (not (= (abs (- fletcher cooper)) 1)))
            (fletcher1 levels)
            (X                         ; baker x cooper
             (lambda (baker cooper) true)
             (baker1 levels)
             (map (lambda (x) (list x))
                  (cooper1 levels))))))))
       ((lambda (s) (map
                (lambda (x) (s s x '(smith miller fletcher baker cooper)))
                smfbc))
        (lambda (s lev names)
          (if (null? lev)
              '()
              (cons (cons (car names)
                          (car lev))
                    (s s
                       (cdr lev)
                       (cdr names)))))))))
  (amb-test '((multiple-dwelling)
              try-again
              try-again
              (multiple-dwelling/scheme)
              ))
  'done)




