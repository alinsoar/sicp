
;;; This is the implementation of the paragraph 3.4 of `Three
;;; Implementation Models for Scheme`, R. Kent Dybvig.  Although this
;;; is simplified comparing to SICP, it covers the details of the tail
;;; calls.

;;; the heap-based model of scheme

(load "dybscmlib")

(define initial-env system-environment)

(define make/label
  (let ((labels '())
        (idx 0))
    (lambda (pointer)
      (cond ((assoc pointer labels) =>
             (lambda (x)
               (cdr x)))
            (else
             (let ((new (number->string idx)))
               (set! labels (cons (cons pointer idx) labels))
               (set! idx (+ 1 idx))
               new))))))

(define compile
  (lambda (exp next)
    (cond ((symbol? exp)
           `(refer ,exp ,next))
          ((pair? exp)
           (case (car exp)
             ((lambda)
              (get/args2
               exp
               (lambda (vars body)
                 `(close ,vars
                         ,(compile body '(return))
                         ,next))))
             ((set!)
              (get/args2
               exp
               (lambda (id val)
                 (compile val
                          `(assign ,id ,next)))))
             ((quote)
              (get/args1
               exp
               (lambda (obj)
                 `(constant ,obj ,next))))
             ((dbg)
              (get/args1
               exp
               (lambda (msg)
                 `(dbg ,msg ,next))))
             ((if)
              (get/args3
               exp
               (lambda (test then else)
                 (compile test
                          `(test ,(compile then next)
                                 ,(compile else next))))))
             ((call/cc)
              (get/args1
               exp
               (lambda (proc)
                 (let ((c `(conti
                            (argument
                             ,(compile proc '(apply))))))
                   (if (tail? next)
                       c
                       `(frame ,next ,c))))))
             (else
              ((lambda (s) (s s
                         (cdr exp)
                         (compile (car exp) '(apply))))
               (lambda (s args c)
                 (if (null? args)
                     (if (tail? next)
                         c
                         `(frame ,next ,c))
                     (s s
                        (cdr args)
                        (compile
                         (car args)
                         `(argument ,c)))))))))
          (else
           `(constant ,exp ,next)))))

(define call-frame
  (lambda (ret env rib stack)
    (list ret env rib stack)))

(define frame-return
  (lambda (stack return)
    (record (take stack 4) return)))

(define lookup-internal
  (lambda (var env)
    (if (null? env)
        (error "unknown variable" var)
        (if (eq? (caar env) var)
            (list (cdar env))
            (lookup-internal var (cdr env))))))

(define print/env
  (lambda (msg env)
    (newline)
    (display msg)
    (newline)
    (for-each
     (lambda (a)
       (for-each
        (lambda (id val)
          (newline)
          (display id)
          (display " ~~ ")
          (display
           (if (not (pair? val))
               val
               ((lambda (s) (s (lambda (flat)
                            ((lambda (s) (s s flat 0))
                             (lambda (s l n)
                               (if (null? l)
                                   '()
                                   (if (= n 3)
                                       '(...)
                                       (cons (car l)
                                             (s s (cdr l) (+ n 1))))))))))
                (lambda (ret)
                  ((lambda (s) (ret (s s val)))
                   (lambda (s flat)
                     (fold-right
                      (lambda (a b)
                        (if (not (pair? a))
                            (cons a b)
                            (append (s s a) b)))
                      '()
                      flat))))))))
        (car a)
        (cadr a))
       (display "\n----------------------"))
     env)
    (display "\n==================================================\n")))

(define lookup
  (lambda (var env)
    (if (null? env)
        (lookup-internal var initial-env)
        (let ((next/rib (car env)))
          (record next/rib
                  (lambda (vars vals)
                    (or (= (length vars)
                           (length vals))
                        (error "lookup vars~vals" vars '-- (length vals)))
                    ((lambda (s) (s s vars vals))
                     (lambda (s vars vals)
                       (if (null? vars)
                           (lookup var (cdr env))
                           (if (eq? var (car vars))
                               vals
                               (s s (cdr vars) (cdr vals))))))))))))

(define extend
  (lambda (vars vals env)
    (cons (list vars vals) env)))

(define apply-closure
  (lambda (closure rib return)
    (record closure
            (lambda (body env vars)
              (return (extend vars rib env)
                      body)))))

(define apply-internal
  (lambda (rator rands)
    (apply rator rands)))

(define closure?
  (lambda (exp)
    (list? exp)))

(define closure
  (lambda (body env vars)
    (list body env vars)))

(define continuation
  (lambda (stack)
    (closure
     `(nuate ,stack vv) '() '(vv))))

(define tail?
  (lambda (exp)
    (eq? 'return (car exp))))

(define vm
  (lambda (acc exp env rib stack)
    ;; (__d "VM:" (car exp))
    (case (car exp)
      ((halt)
       (or (null? stack)
           (begin (display "STACK IS NOT NULL")
                  (newline)))
       acc)
      ((frame)
       (get/args2
        exp
        (lambda (ret code)
          (vm acc code env '() (call-frame ret env rib stack)))))
      ((argument)
       (get/args1
        exp
        (lambda (next)
          (vm acc next env (cons acc rib) stack))))
      ((refer)
       (get/args2
        exp
        (lambda (var next)
          '(and (eq? 's var)
                (__d "\nREF:" (car (lookup var env)) "\n"))
          (vm (car (lookup var env))
              next env rib stack ))))
      ((constant)
       (get/args2
        exp
        (lambda (obj next)
          (vm obj next env rib stack))))
      ((apply)
       (if (closure? acc)
           (apply-closure acc rib
                          (lambda (env2 body)
                            (vm acc body env2 '() stack)))
           (frame-return
            stack
            (lambda (next env rib2 stack)
              ;; because internal operators are not a closure,
              ;; `return` must be executed here.
              (vm (apply-internal acc rib) next env rib2 stack)))))
      ((close)
       (get/args3
        exp
        (lambda (vars body next)
          ;; (dbg/closure vars body env)
          (vm (closure body env vars) next env rib stack))))
      ((return)
       ;; (print/env "dbg:RETURN" env)
       (frame-return stack (lambda (next env rib stack)
                             (vm acc next env rib stack))))
      ((assign)
       (get/args2
        exp
        (lambda (id next)
          (set-car! (lookup id env) acc)
          ;;(vm acc next env rib stack)
          )))
      ((test)
       (get/args2
        exp
        (lambda (then else)
          (vm acc (if acc then else) env rib stack))))
      ((dbg)
       (get/args2
        exp
        (lambda (msg next)
          ;; (print/env msg env)
          (vm acc next env rib stack))))
      ((conti)
       (get/args1
        exp
        (lambda (next)
          (vm (continuation stack) next env rib stack))))
      ((nuate)
       (get/args2
        exp
        (lambda (stack var)
          (vm (car (lookup var env))
              '(return) env rib stack))))
      (else (error "unknown instruction" exp)))))

(define evaluate
  (lambda (dbg)
    (lambda (code)
      ;; (display "\n\n=================") (newline)
      (let ((kernel (desugar code)))
        ;; (display "@") (pretty-print kernel) (display "@") (newline)
        (let ((asm (compile kernel '(halt))))
          (and dbg (pretty-print asm) (newline))
          (display "RESULT:")
          (pretty-print (vm 'acc/unbound asm '() '() '()))
          (newline))))))

(define eval (evaluate false))
(define eval2 (evaluate true))



(newline)

(eval 200)

(eval ''ok)

(eval '''ok)

(eval
 '(+ 100 100))

(eval
 '(lambda (x) x x))

(eval
 '((lambda (x) x)
   88))

(eval
 '((lambda (x) (+ 2 x))
   77))

(eval
 '(begin 1 2 3))

(eval
 '((lambda (x)
     (set! x (+ x 1))
     (+ x x))
   5))

(eval
 '(if (= 0 0)
           1
           2))

(eval
 '((lambda (x)
     'x
     (if (= x 0)
         444
         (begin
           (set! x (- x 1))
           (+ x x))))
   10))

(eval
 '((lambda (____)
     ((lambda (____)
        3)
      2))
   1))

(eval '((lambda (a b) (+ a b))
        1 2))

(eval '((lambda (a b) b)
        1 2))

(eval ''((lambda (s) (s s))
         (lambda (s) (s s))))

(eval '((lambda (t f)
          (if (= 3 3)
              (t)
              (f)))
        (lambda () 'true)
        (lambda () 'false)))

(eval '((lambda (t f)
          (if (= 3 9)
              (t 7)
              (f 8)))
        (lambda (a) a)
        (lambda (a) a)))

(eval '((lambda (s) (s s 10 0))
         (lambda (s n m)
           (if (= n 0)
               m
               ((lambda (n-1)
                  ((lambda (m+n)
                     (s s n-1 m+n))
                   (+ n m)))
                (- n 1))))))

(eval '(call/cc
         (lambda (cc)
           10)))

(eval '(call/cc
         (lambda (cc)
           (+ 2
              3
              (cc 22)))))

(eval ''((lambda (cc)
           "an infinite loop"
           (cc cc))
        (call/cc
         (lambda (cc) cc))))

(eval '((lambda (n fact)
          ((lambda (cc)
             (set! fact (* fact n))
             (set! n (- n 1))
             (if (= n 0)
                 fact
                 (cc cc)))
           (call/cc
            (lambda (cc) cc))))
        10
        1))

(eval2 '(+ 1 2
           (* 5 7)))

(eval '((lambda (x)
           (+ 1 x))
         10))

(eval '(+ 2 (* 3 5)))

(eval '(+ 2
          (* 3 9)))

(define call/cc call-with-current-continuation)

(eval
 '(call/cc
   (lambda (break)
     ((lambda (s) (s s 10 break))
      (lambda (s n cc)
        (if (= 0 n)
            (cc 0)
            ((lambda (n-1)
               (+ n
                  (call/cc
                   (lambda (__)
                     (s s n-1 __)))))
             (- n 1))))))))

(eval
 '((lambda (s) (s s 10))
   (lambda (s k)
     (if (= 0 k)
         0
         (+ k (s s (- k 1)))))))

(eval2
 '((lambda (count)
     ((lambda (s) (s s 10))
      (lambda (s k)
        ;; (dbg)
        (set! count (+ 1 count))
        (if (= 0 k)
            0
            ((lambda (v w)
               (+ k v))
             ((lambda (v) v)
              (s s (- k 1)))
             ((lambda (v) v)
              (s s (- k 1))))))))
   0))

(eval2
 '((lambda (s) (s s 1000))
   (lambda (s k)
     (if (= 0 k)
         0
         (+ k
            ((lambda (v) v)
             (s s (- k 1))))))))

(eval2
 '((lambda (s) (s s 10))
   (lambda (s k)
     (if (= 0 k)
         0
         (+ k
            (s s (- k 1)))))))

;;; factorial(100) computed iteratively
;; RESULT:93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000
(eval '((lambda (n)
          ((lambda (s) (s s 1 1))
           (lambda (s product counter)
             (if (> counter n)
                 product
                 (s s (* counter product) (+ counter 1))))))
         100))

