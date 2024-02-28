;;; -*- mode:scheme; buffer-read-only:nil -*-

;;; This is the heap-based evaluator from section 3.5 ("Improving
;;; variable access") from the thesis "Three implementation models for
;;; the Scheme Programming Language" by Kent Dybvig.  While this
;;; scheme system is simplified comparing to SICP, it covers the same
;;; details about eliminating variable names and replacing them with
;;; (x.y) coordinates.

;;; The remark #46 from SICP,

;;   46 This is the modification to variable lookup required if we
;;      implement the scanning method to eliminate internal
;;      definitions (exercise 5.43). We will need to eliminate these
;;      definitions in order for lexical addressing to work.

;;; is not correct, and Dybvig's thesis uses only indexes for all
;;; variables apart from those that are mutated with set!, for
;;; closures and for continuations.

;;; improve variable access

;;; the compiler has an environment and for each variable access it
;;; will generate the index of the variable of the form (rib.idx) such
;;; that the virtual machine to be able to find the variable faster.

(load "dybscmlib")

;;; ______________________________ $ ______________________________

(define initial-env
  `(( ,(map car system-environment)
      ,(map (lambda (x) (cons 'internal (cdr x))) system-environment))))

(define tail-recursive?
  (lambda (exp)
    (eq? ':__RETURN (car exp))))

(define compile-static-address
  (lambda (sym env return)
    ((lambda (s) (s s env 0))
     (lambda (u env rib)
       (if (null? env)
           (begin
             (display "unknwon symbol: ")
             (display sym)
             (exit 0))
           ((lambda (s) (s s (car env) 0))
            (lambda (s e idx)
              (if (null? e)
                  (u u (cdr env) (+ 1 rib))
                  (if (eq? sym (car e))
                      (return rib idx)
                      (s s (cdr e) (+ idx 1)))))))))))

(define extend
  (lambda (env params/values)
    (cons params/values env)))

(define compile
  (lambda (exp env next)
    (cond ((symbol? exp)
           (compile-static-address
            exp env
            (lambda (rib idx)
              `(:__REFER ,rib ,idx ,next))))
          ((pair? exp)
           (case (car exp)
             ((quote)
              (case/record1
               exp
               (lambda (obj)
                 `(:__CONSTANT ,obj ,next))))
             ((lambda)
              (case/record2
               exp
               (lambda (params body)
                 `(:__CLOSE ,(compile body
                                      (extend env params)
                                      '(:__RETURN))
                            ,next))))
             ((if)
              (case/record3
               exp
               (lambda (test then else)
                 (compile test env
                          `(:__TEST ,(compile then env next)
                                   ,(compile else env next))))))
             ((set!)
              (case/record2
               exp (lambda (id val)
                     (compile-static-address
                      id env
                      (lambda (rib idx)
                        (compile val env `(:__ASSIGN ,rib ,idx ,next)))))))
             ((call/cc)
              (case/record1
               exp
               (lambda (proc)
                 (let ((c `(:__CONTI
                            (:__ARGUMENT
                             ,(compile proc env '(:__APPLY))))))
                   (if (tail-recursive? next)
                       c
                       `(:__FRAME ,next ,c))))))
             (else
              ;; APPLY
              ((lambda (s) (s s (cdr exp)
                         (compile (car exp) env '(:__APPLY))))
               (lambda (s args c)
                 (if (null? args)
                     (if (tail-recursive? next)
                         c
                         `(:__FRAME ,next ,c))
                     (s s (cdr args)
                        (compile (car args) env
                                 `(:__ARGUMENT ,c)))))))))
          (else
           `(:__CONSTANT ,exp ,next) ) ) ) )

;;; ______________________________ $ ______________________________

(define internal-procedure?
  (lambda (e)
    (and (pair? e)
         (eq? 'internal (car e))
         (procedure? (cdr e)))))

(define combination?
  (lambda (e)
    (and (pair? e)
         (eq? 'combination (car e)))))

(define call-frame
  (lambda (ret env rib stack)
    (list ret env rib stack)))

(define frame-return
  (lambda (stack return)
    (record4 (take stack 4) return)))

(define lexical-address-lookup
  (lambda (rib idx env)
    ((lambda (s) (s s rib env))
     (lambda (s rib env)
       (if (zero? rib)
           ((lambda (s) (s s idx (car env)))
            (lambda (s idx vals)
              (if (zero? idx)
                  vals
                  (s s (- idx 1) (cdr vals)))))
           (s s (- rib 1) (cdr env)))))))

(define lexical-address-set!
  (lambda (rib idx :E :A)
    (set-car! (lexical-address-lookup rib idx :E) :A)))

(define closure
  (lambda (body env)
    (list 'combination body env)))

(define continuation
  (lambda (stack)
    (closure `(:__NUATE ,stack) '() )))

(define vm
  (lambda (:A :X :E :R :S)
    (case (car :X)
      ((:__HALT)
       (or (null? :S)
           (begin (display "STACK IS NOT NULL")
                  (newline)))
       (HALT/CHECK 'HALT!)
       :A)
      ((:__CONSTANT)
       (case/record2
        :X
        (lambda (obj next)
          (vm obj next :E :R :S) ) ) )
      ((:__REFER)
       (case/record3
        :X
        (lambda (rib idx next)
          (vm (car (lexical-address-lookup rib idx :E)) next :E :R :S) ) ) )
      ((:__CLOSE)
       (case/record2
        :X
        (lambda (body next)
          (vm (closure body :E) next :E :R :S))))
      ((:__FRAME)
       (case/record2
        :X
        (lambda (ret.addr next)
          (vm :A next :E '() (call-frame ret.addr :E :R :S)))))
      ((:__ARGUMENT)
       (case/record1
        :X
        (lambda (next)
          (vm :A next :E (cons :A :R) :S))))
      ((:__APPLY)
       (if (internal-procedure? :A)
           ;; Who is `NEXT` for application of an internal procedure?
           (frame-return
            :S (lambda (next env rib stack)
                 (vm (apply (cdr :A) :R) next env rib stack)))
           (if (combination? :A)
               (record2
                (cdr :A)
                (lambda (body env)
                  (vm :A body (extend env :R) '() :S) ) )
               (error "apply: unknown object" :A) ) ) )
      ((:__TEST)
       (case/record2
        :X
        (lambda (then else)
          (vm :A (if :A then else) :E :R :S))))
      ((:__ASSIGN)
       (case/record3
        :X
        (lambda (rib idx next)
          (lexical-address-set! rib idx :E :A)
          (vm :A next :E :R :S) ) ) )
      ((:__RETURN)
       (frame-return :S (lambda (next env rib stack)
                          (vm :A next env rib stack))))
      ((:__CONTI)
       (case/record1
        :X
        (lambda (next)
          (vm (continuation :S) next :E :R :S))))
      ((:__NUATE)
       (case/record1
        :X
        (lambda (continuation/stack)
          (vm (car (lexical-address-lookup 0 0 :E)) '(:__RETURN) :E :R continuation/stack))))
      (else (error "unknown instruction" :X) ) ) ) )

;;; ______________________________ $ ______________________________

(define eval
  (lambda (exp)
    (HALT/CHECK 'START)
    (let ((kernel (desugar exp)))
      ((lambda (s) (s s 40))
       (lambda (s n) (if (zero? n) (newline) (begin (display "* ") (s s (- n 1))))))
      (pp kernel)
      (let ((asm (compile kernel (map car initial-env) '(:__HALT))))
        (display "~~~>\n")
        (pp asm)
        (newline)
        (display ": ")
        (display (vm 'unbound asm (map cadr initial-env) '() '()))
        (HALT/CHECK 'HALTED?)
        (newline)))))

;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;; TESTS
;;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(eval '100)
(eval '*)
(eval '(lambda (a b) (lambda (c d) (+ a b c d))))
(eval '((lambda (a b) ((lambda (c d) (+ a b c d)) 2 4)) 6 8))
(eval '((lambda (s) (s s 1000)) (lambda (s k) (if (= 0 k) 0 (+ k (s s (- k 1)))))))
(eval '((lambda (x) (set! x (+ x 1)) (- x 2)) 10))
(eval '((lambda (a b) ((lambda (c d) (set! a (+ a b))) 2 4) a) 10 25))
(eval '((lambda (s) (s s)) (lambda () 11)))
(eval '(call/cc
        (lambda (break)
          ((lambda (s) (s s 100 break))
           (lambda (s n cc)
             (if (= 0 n)
                 (cc 0)
                 (+ n
                    (call/cc
                     (lambda (__)
                       (s s (- n 1) __))))))))))
(eval '(call/cc
        (lambda (break)
          ((lambda (s) (s s 100 break))
           (lambda (s n cc)
             (if (= 0 n)
                 (cc 0)
                 (cc (+ n
                        (call/cc
                         (lambda (__)
                           (s s (- n 1) __)))))))))))
(eval '(+ 1 2 3))
(eval '((lambda () (+ 2 2) (+ 2 3))))
(eval '(+ 2 (* 3 4) 6))
(eval '((lambda (a return) (return a (+ a 1))) 10 (lambda (a b) (cons a b))))
(eval '((lambda (a) (~ "OK!\n") a) 20))
(eval ' ((lambda (f1 f2 a) (f1 f2 a)) (lambda (f2 n) (f2  (+ n 1))) (lambda (n) (+ n n)) 10))
(eval '((lambda (f1) (f1)) (lambda () 'x)))
(eval '((lambda (f1 f2) (f1 f2))
        (lambda (f2) (f2))
        (lambda () 'x)))
(eval '((lambda (a) (+ a a)) 10))

