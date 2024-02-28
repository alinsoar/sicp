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
     (define (distinct? l)
       (define (iter l)
         (cond ((null? l) true)
               ((member? (car l) (cdr l)) false)
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
     (define (foldr op init l)
       (if (null? l)
           init
           (op (car l)
               (foldr op init (cdr l)))))
     (define (all l)
       (if (null? l)
           true
           (if (car l)
               (all (cdr l))
               false)))
     (define (yacht-and-daughter)
       (let ((daughters '(Lorna
                          Melissa
                          Rosalind
                          Gabrielle
                          MaryAnnMoore)))
         (define y car)
         (define d cdr)
         (define father/yacht/daughter
           (foldr
            (lambda (yacht r)
              (append (map (lambda (daughter) (cons yacht daughter))
                           daughters)
                      r))
            '()
            daughters))

         (define (fyd)
           (define (iter l)
             (if (null? l)
                 (amb)
                 (amb (car l) (iter (cdr l)))))
           (iter father/yacht/daughter))

         (let ((Mr.Moore        (fyd)))
           (require (eq? (y Mr.Moore) 'Lorna))
           (require (eq? (d Mr.Moore) 'MaryAnnMoore))
           (let ((SirBarnacleHood (fyd)))
             (require (eq? (y SirBarnacleHood) 'Gabrielle))
             (require (eq? (d SirBarnacleHood) 'Melissa))
             (let ((ColonelDowning  (fyd)))
               (require (eq? (y ColonelDowning) 'Melissa))
               (let ((Mr.Hall         (fyd)))
                 (require (eq? (y Mr.Hall) 'Rosalind))
                 (let ((Dr.Parker       (fyd)))
                   (require (eq? (y Dr.Parker) 'MaryAnnMoore))
                   
                   (define res (list Mr.Moore
                                     ColonelDowning
                                     Mr.Hall
                                     SirBarnacleHood
                                     Dr.Parker ))

                   (require (all
                             (map
                              (lambda (yd) (if (eq? 'Gabrielle (d yd))
                                          (eq? (y yd) (d Dr.Parker))
                                          true))
                              res)))
                   
                   (require (distinct?
                             (map
                              d
                              res)))
                   
                   (require (all
                             (map
                              (lambda (yd) (not (eq? (y yd) (d yd))))
                              res)))

                   (list (cons 'Mr.Moore Mr.Moore)
                         (cons 'ColonelDowning ColonelDowning)
                         (cons 'Mr.Hall Mr.Hall)
                         (cons 'SirBarnacleHood SirBarnacleHood)
                         (cons 'Dr.Parker Dr.Parker)))))))))))
  (amb-test '((yacht-and-daughter)
              try-again
              try-again
              ))
  'done)
