#lang racket

(require (submod "e.2.73.rkt" export))

(define (install-records-file-1)
  ;; encoding for file 1: NAME ADDRESS SALARY
  (let ((records '(( george  george_address  10 )
                   ( john john_address 20 )
                   ( abraham abraham_address 30 )
                   ( jay jay_address 40 )
                   ( hal hal_address 50 )
                   ( julie julie_address 60 )
                   ( adelle adelle_address 70 )
                   ( andrew andrew_address 80 )
                   )))
    (define (get-record name)
      (define (iter l)
        (cond ((null? l) false)
              ((eq? name (caar l)) (car l))
              (else (iter (cdr l)))))
      (iter records))
    (define (get-salary name)
      (define (iter l)
        (cond ((null? l) false)
              ((eq? name (caar l)) (caddar l))
              (else (iter (cdr l)))))
      (iter records))
    (put 'get-record 'file1 get-record)
    (put 'get-salary 'file1 get-salary)))
(define (install-records-file-2)
  ;; different encoding for file 2: NAME SALARY ADDRESS
  (let ((records '(( george (lambda (x) (x 1))  george_address )
                   ( john (lambda (x) (x 2)) john_address )
                   ( abraham (lambda (x) (x 3)) abraham_address )
                   ( jay (lambda (x) (x 4)) jay_address )
                   ( hal (lambda (x) (x 5)) hal_address )
                   ( julie (lambda (x) (x 6)) julie_address )
                   ( adelle (lambda (x) (x 7)) adelle_address )
                   ( andrew (lambda (x) (x 8)) andrew_address )
                   )))
    (define (get-record name)
      (define (iter l)
        (cond ((null? l) false)
              ((eq? name (caar l))
               (list (caar l)
                     (caddar l)
                     ((eval (cadar l))
                      (lambda (x) (* x 100)))))
              (else (iter (cdr l)))))
      (iter records))
    (define (get-salary name)
      (define (iter l)
        (cond ((null? l) false)
              ((eq? name (caar l))
               ((eval (cadar l))
                (lambda (x) (* x 100))))
              (else (iter (cdr l)))))
      (iter records))
    (put 'get-record 'file2 get-record)
    (put 'get-salary 'file2 get-salary)))

;;; we supply the file and name for each operation. We index all the
;;; files after NAME field.
(define (get-record file name) ((get 'get-record file) name))
(define (get-salary file name) ((get 'get-salary file) name))

(define (find-employee-record name files)
  (map (lambda (f) (get-record f name))
       files))

(module+ test
  (install-records-file-1)
  (install-records-file-2)
  '--test-file1--
  (get-record 'file1 'john)
  (get-salary 'file1 'julie)
  '--test-file2--
  (get-salary 'file2 'julie)
  (get-salary 'file2 'martin)             ; no martin in file2
  '--search-in-all-files--
  (find-employee-record 'julie '(file1 file2)))


