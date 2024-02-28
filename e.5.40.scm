
;;; This is the stack-based evaluator from chapter 4 from the thesis
;;; "Three implementation models for the Scheme Programming Language"
;;; by Kent Dybvig.

;;; features:
;;;
;;; ~ snapshot continuations (copy the stack inside continuation object)
;;; ~ allocation of the static chain inside closure objects
;;; ~ direct value of immutable variable
;;; ~ boxed value of mutable variables
;;; ~ display closures allocated in the heap
;;; ~ tail calls by shifting arguments on the stack
;;; ~ 

(load "dybscmlib")

(define tail?
  (lambda (exp)
    (eq? 'return (car exp))))

(define shift-args
  (lambda (n m :Stack)
    (for-each (lambda (i)
                (:Stack 'index-set! 'sp (+ i m) (:Stack 'index 'sp i)))
              (iota n (- n 1) -1))
    (:Stack 'pop m)))

;; returns the set of variables in v that are assigned in exp.
(define find-sets
  (lambda (exp v)
    (cond ((symbol? exp)
           '())
          ((pair? exp)
           (case (car exp)
             ((quote)
              '())
             ((lambda)
              (get/args2
               exp
               (lambda (vars body)
                 (find-sets body (set-minus v vars)))))
             ((if)
              (get/args3
               exp
               (lambda (test then else)
                 (set-union (find-sets test v)
                            (set-union (find-sets then v)
                                       (find-sets else v))))))
             ((set!)
              (get/args2
               exp
               (lambda (var val)
                 (set-union (if (set-member? var v)
                                (list var)
                                '())
                            (find-sets val v)))))
             ((call/cc)
              (get/args1
               exp
               (lambda (f)
                 (find-sets f v))))
             (else
              ((lambda (s) (s s exp))
               (lambda (s exp)
                 (if (null? exp)
                     '()
                     (set-union (find-sets (car exp) v)
                                (s s (cdr exp)))))))))
          (else
           '()))))

;; sets = list of assigned variables
;; vars = a list of arguments
;; n counts the arguments; the argument number is used by the
;; box instruction to access the correct stack location
(define make-boxes
  (lambda (sets vars next)
    ((lambda (s) (s s vars 0))
     (lambda (s vars n)
       (if (null? vars)
           next
           (if (set-member? (car vars) sets)
               (list 'box n
                     (s s (cdr vars) (+ n 1)))
               (s s (cdr vars) (+ n 1))))))))

(define box
  (lambda (x)
    (vector x)))

(define unbox
  (lambda (x)
    (vector-ref x 0)))

(define set-box!
  (lambda (b v)
    (vector-set! b 0 v)))

(define system-env-symbols
  (map car system-environment))

(define extend
  (lambda (e r)
    (cons r e)))

(define display-closure
  (lambda (body n :Stack)
    (let ((closure (make-vector (+ n 1))))
      (vector-set! closure 0 body)
      (for-each (lambda (i)
                  (vector-set! closure (+ i 1) (:Stack 'index 'sp i)))
                (iota n))
      closure)))

(define closure-body
  (lambda (closure)
    (vector-ref closure 0)))

(define index-closure
  (lambda (closure n)
    (vector-ref closure (+ n 1))))

(define internal/op?
  (lambda (op)
    (procedure? op)))

(define continuation
  (lambda (:Stack)
    (display-closure `(refer-local 0 (nuate ,(:Stack 'save)
                                            (return 0)))
                     0
                     '__)))

;; collect free variables for inclusion in the closure. collect-free
;; arranges to push the value of each free variable in turn on the
;; stack (using the ARGUMENT instruction)
(define collect-free
  (lambda (vars env next)
    (if (null? vars)
        next
        (collect-free (cdr vars)
                      env
                      (compile-refer (car vars) env
                                     `(argument ,next))))))

(define find-free
  (lambda (exp bound)
    ;; (__d "|" exp "|" bound)
    (cond ((symbol? exp)
           (if (set-member? exp bound)
               '()
               (list exp)))
          ((pair? exp)
           (case (car exp)
             ((quote)
              '())
             ((lambda)
              (get/args2
               exp
               (lambda (vars body)
                 (find-free body (set-union vars bound)))))
             ((if)
              (get/args3
               exp
               (lambda (test then else)
                 (set-union (find-free test bound)
                            (set-union (find-free then bound)
                                       (find-free else bound))))))
             ((set!)
              (get/args2
               exp
               (lambda (var val)
                 (set-union (if (set-member? var bound)
                                '()
                                (list var))
                            (find-free val bound)))))
             ((call/cc)
              (get/args1
               exp
               (lambda (f)
                 (find-free f bound))))
             (else
              ((lambda (s) (s s exp))
               (lambda (s exp)
                 (if (null? exp)
                     '()
                     (set-union (find-free (car exp) bound)
                                (s s (cdr exp)))))))))
          (else
           '()))))

(define compile-system-environment-reference
  (lambda (sym return)
    (if (memq sym system-env-symbols)
        (return sym)
        (begin
          (__d "unknown symbol: " sym)
          (exit 0)))))

;; takes two return arguments,one for when it locates a local
;; variable, one for free variables, and this version only has to look
;; at two levels; either a variable is in the list of local variables
;; or it is the list of free variables.

;; a compile-time environment is a pair whose car is the list of local
;; variables and whose cdr is the list of free variables.
(define compile-lookup
  (lambda (sym env return-local return-free return-system)
    (if (null? env)
        (compile-system-environment-reference sym return-system)
        ((lambda (s) (s s (car env) 0))
         (lambda (s locals n)
           ;; try locals
           (if (null? locals)
               ((lambda (s) (s s (cdr env) 0))
                (lambda (s free n)
                  ;; try free
                  (if (null? free)
                      ;; try system environment
                      (compile-system-environment-reference sym return-system)
                      (if (eq? sym (car free))
                          (return-free n)
                          (s s (cdr free) (+ n 1))))))
               (if (eq? sym (car locals))
                   (return-local n)
                   (s s (cdr locals) (+ n 1)))))))))

;; variables may be found on the stack in the current call frame or in
;; the closure of the current function.  it handles references to two
;; types of variables: those local to the current call frame and those
;; stored in the display closure of the active function
(define compile-refer
  (lambda (exp env next)
    (compile-lookup exp env
                    (lambda (n) (list 'refer-local n next))
                    (lambda (n) (list 'refer-free n next))
                    (lambda (n) (list 'refer-system n next)))))

;;; sets argument telling the compiler what free variables are mutated
(define compile
  (lambda (exp env sets next)
    ;; (__d "\n?exp?" exp "\n?nex?" next "\n?env?" env "\n?set?" sets)
    (cond ((symbol? exp)
           ;; for each boxed argument an indirect is generated when
           ;; it's referenced.
           (compile-refer exp env
                          (if (set-member? exp sets)
                              `(indirect ,next)
                              next)))
          ((pair? exp)
           (case (car exp)
             ((quote)
              (get/args1
               exp
               (lambda (obj)
                 `(constant ,obj ,next))))
             ((if)
              (get/args3
               exp
               (lambda (test then else)
                 (let ((then (compile then env sets next))
                       (else (compile else env sets next)))
                   (compile test env sets `(test ,then ,else))))))
             ((set!)
              (get/args2
               exp
               (lambda (var val)
                 (compile-lookup
                  var env
                  (lambda (n) (compile val env sets `(assign-local ,n ,next)))
                  (lambda (n) (compile val env sets `(assign-free ,n ,next)))
                  (lambda (n) (compile val env sets `(assign-system ,n ,next)))))))
             ((lambda)
              (get/args2
               exp
               (lambda (vars body)
                 (let ((free (find-free body (append vars system-env-symbols)))
                       (sets2 (find-sets body (append vars system-env-symbols))))
                   ;; sets is the subset of the VARS that are mutated
                   (collect-free
                    free env
                    `(close ,(length free)
                            ,(make-boxes sets2 vars
                                         (compile body
                                                  (cons vars free)
                                                  (set-union sets2
                                                             (set-intersect sets free))
                                                  `(return ,(length vars))))
                            ,next))))))
             ((call/cc)
              (get/args1
               exp
               (lambda (f)
                 (let ((c `(conti
                            (argument
                             ,(compile f env sets
                                       (if (tail? next)
                                           (get/args1
                                            next
                                            (lambda (n)
                                              `(shift 1 ,n (apply '_))))
                                           '(apply '_)))))))
                   (if (tail? next)
                       c
                       `(frame ,next ,c))))))
             (else
              ;; apply
              ;; ==============================
              ;; no more STATIC LINK
              ;; ARG1                       ~ <== ARGUMENT LIST
              ;; ....                       ~ 
              ;; ARGn                       ~ 
              ;; NEXT EXPR(RET ADDRESS)     | <== FRAME CONTENT
              ;; FRAME POINTER              |
              ;; STACK POINTER              |
              (let ((rator (compile (car exp) env sets
                                    (if (tail? next)
                                        (get/args1
                                         next
                                         (lambda (n)
                                           `(shift ,(length (cdr exp))
                                                   ,n
                                                   (apply ,(length (cdr exp))))))
                                        `(apply ,(length (cdr exp))))))
                    (rands (cdr exp)))
                ((lambda (s) (s s rator rands))
                 (lambda (s c args)
                   (if (null? args)
                       (if (tail? next)
                           c
                           `(frame ,next ,c))
                       (s s
                          (compile (car args) env sets `(argument ,c))
                          (cdr args)))))))))
          (else
           `(constant ,exp ,next)))))

(define vm
  (lambda (acc exp closure :Stack)
    ;; (__d "\n acc:" acc "\n exp:" exp)
    ;; (:Stack 'print)
    (case (car exp)
      ((halt)
       acc)
      ((constant)
       (get/args2
        exp
        (lambda (obj next)
          (vm obj next closure :Stack))))
      ((close)
       (get/args3
        exp
        (lambda (n/free body next)
          (let ((clos (display-closure body n/free :Stack)))
           (vm clos
               next closure
               (:Stack 'pop n/free))))))
      ((frame)
       (get/args2
        exp
        (lambda (ret next)
          (vm acc next closure
              (:Stack 'push closure (:Stack 'fp) ret)))))
      ((argument)
       (get/args1
        exp
        (lambda (next)
          (vm acc next closure (:Stack 'push acc)))))
      ((refer-local)
       (get/args2
        exp
        (lambda (n next)
          (vm (:Stack 'index 'fp n)
              next closure :Stack))))
      ((refer-system)
       (get/args2
        exp
        (lambda (x next)
          (vm (cdr (assoc x system-environment))
              next closure :Stack))))
      ((refer-free)
       (get/args2
        exp
        (lambda (n next)
          (vm (index-closure closure n)
              next closure :Stack))))
      ((apply)
       (if (internal/op? acc)
           (get/args1
            exp
            (lambda (args/count)
              (let ((args ((lambda (s) (s s 0))
                           (lambda (s n)
                             (let ((x (:Stack 'index 'sp n)))
                               (if (< n args/count)
                                   (cons x (s s (+ n 1)))
                                   '()))))))
                ;; similar to a return
                (:Stack 'pop args/count)
                (let ((ret  (:Stack 'index 'sp 0))
                      (fp   (:Stack 'index 'sp 1))
                      (clos (:Stack 'index 'sp 2)))
                  (:Stack 'set-fp! fp)
                  (vm (apply acc args) ret clos (:Stack 'pop 3))))))
           (vm acc (closure-body acc) acc (:Stack 'set-fp! (:Stack 'sp)))))
      ((return)
       (get/args1
        exp
        (lambda (n)
          (:Stack 'pop n)
          (let ((ret  (:Stack 'index 'sp 0))
                (fp   (:Stack 'index 'sp 1))
                (clos (:Stack 'index 'sp 2)))
            (:Stack 'set-fp! fp)
           (vm acc ret clos (:Stack 'pop 3))))))
      ((assign-local)
       (get/args2
        exp
        (lambda (n next)
          (set-box! ( :Stack 'index 'fp n) acc)
          (vm acc next closure :Stack))))
      ((assign-free)
       (get/args2
        exp
        (lambda (n next)
          (set-box! (index-closure closure n) acc)
          (vm acc next   closure :Stack))))
      ((test)
       (get/args2
        exp
        (lambda (then else)
          (vm acc (if acc then else) closure :Stack))))
      ((conti)
       (get/args1
        exp
        (lambda (next)
          (vm (continuation :Stack) next closure :Stack))))
      ((nuate)
       (get/args2
        exp
        (lambda (stack next)
          (vm acc next closure (:Stack 'restore stack)))))
      ((box)
       (get/args2
        exp
        (lambda (n next)
          (vm acc next closure (:Stack 'index-set! 'sp n (box (:Stack 'index 'sp n)))))))
      ((indirect)
       (get/args1
        exp
        (lambda (next)
          (vm (unbox acc) next closure :Stack))))
      ((shift)
       (get/args3
        exp
        (lambda (n m next)
          (vm acc next closure (shift-args n m :Stack)))))
      (else
       (error "vm: unknown instruction" (car exp))))))

(define evaluate
  (lambda (e)
    ;; (set! stack/vector (mk/stack/vector))
    (display "\n~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~\n") (pp e) (newline)
    (let ((c (compile e '() '() '(halt))))
      (display "\n~~ ~~\n")
      (pp c)
      (let* ((:Stack (mk/stack STACK/LENGTH))
             (res (vm 'unbound c '() :Stack)))
        (newline)
        (display "> ")
        (pp res)
        (:Stack 'stat)
        (newline)))))

;;; TESTS

'(evaluate ''(+ 2 3))

'(evaluate '(lambda (x) x))

'(evaluate '(if (= 10 20) 'a 'b))
'(evaluate '(if (= 10 10) 'a 'b))

;; same result as doing so:
;; mit-scheme << END
;; (pp -)
;; END
;; or doing so:
;; echo '(pp -)' | mit-scheme
'(evaluate
 '((lambda (x y z)
     ;; "pretty-print will print the code of `-`"
     x)
   -
   20
   33))

;;; n*(n+1)/2 for n=100
(evaluate '((lambda (s) (s s 100))
            (lambda (s n)
              (if (= 0 n)
                  0
                  (+ n
                     (s s (- n 1)))))))

;;; accumulator passing style
(evaluate '((lambda (s) (s s 100 0))
            (lambda (s n sum)
              (if (= 0 n)
                  sum
                  (s s (- n 1) (+ sum n))))))

;;; continuation passing style
(evaluate '((lambda (s) (s s 100 (lambda (sum) sum)))
            (lambda (s n col)
              (if (= 0 n)
                  (col 0)
                  (s s (- n 1)
                     (lambda (sum)
                       (col (+ sum n))))))))

(evaluate '((lambda (n)
              n)
            10))

(let ((c '(frame
            (halt)
            (constant
             1111
             (argument (close 0
                              (frame (return 1)
                                     (refer-local 0
                                                  (argument
                                                   (close 1
                                                          (refer-free 0 (return 0))
                                                          (apply 0)))))
                              (apply 1)))))))

  (__d "." (vm 'unbound c '() (mk/stack STACK/LENGTH))))

(evaluate '((lambda (n)
              ((lambda ()
                 n)))
            10))

(evaluate ' ((lambda () +)))

(evaluate '((lambda (n)
              ((lambda ()
                 (* 2 n))))
            10))

(evaluate '((lambda (x y)
              ((lambda (z)
                 ;; 20+44
                 (+ y z))
               ;; z = 44
               (+ x 1 33)))
            10
            20))

(evaluate '((lambda (x)
              ;; x = 11 ; x+F+x = 22+25 = 47
              (+ x
                 ((lambda (x)
                    ;; x = 12 ; F=x+G = 27
                    (+ x
                       ((lambda (x)
                          ;; x = 12
                          (+ x 3))
                        x)))
                  (+ 1 x))
                 x))
            11))

;;(evaluate '(+ 9 11))

;;; non-iterative process
(evaluate
 '((lambda (s) (s s 100))
   (lambda (s n)
     (if (= 0 n)
         0
         ((lambda (n-1)
            (+ n (s s n-1)))
          (- n 1))))))

(evaluate '2)

(evaluate '(lambda (x) x))
(evaluate '(lambda (_) +))
(evaluate '(lambda (+) +))

(evaluate '((lambda (x) x)
            10))

(evaluate '((lambda (_) +)
            10))

(evaluate '((lambda (x) (+ x 1))
            10))

(evaluate '(lambda (x y) 
             (lambda (z t)
               (+ x y z t 1))))

(evaluate '((lambda (x) (+ x 1))
            10))

(evaluate '(= 1 2))

(evaluate '(= 1 1))

;;; recursive process
(evaluate '((lambda (s) (s s 100))
            (lambda (s n)
              (if (= n 0)
                  0
                  (+ n (s s (- n 1)))))))

(evaluate '((lambda (s) (s s 10 (lambda (n) n)))
            (lambda (s n col)
              (if (= n 0)
                  (col 0)
                  (s s (- n 1)
                     (lambda (rest)
                       (col (+ rest n))))))))

(evaluate '((lambda (s0)
              ((lambda (s1)
                 (+ s1 s0))
               (+ 10 s0)))
            10))

(evaluate '(+ 2
              (call/cc
               (lambda (break)
                 (+ 3 4 (break 100) 5)))
              10))

(let ((c '(frame (halt)
                 (conti (argument
                         (close 0
                                (frame (return 1)
                                       (constant 'xyz (argument (refer-local 0 (apply 1)))))
                                (apply '_)))))))
  (__d "." (vm 'unbound c '() (mk/stack STACK/LENGTH))))

(evaluate '(call/cc
            (lambda (break)
              (break 777))))

(evaluate '((lambda (a)
              ((lambda ()
                 (set! a 20)))
              a)
            10))

(evaluate '((lambda (a)
              (set! a 20)
              a)
            10))

(evaluate '((lambda (x)
              (set! x (+ x 1))
              x)
            10))

(evaluate '((lambda (sum)
              ((lambda ()
                 ((lambda (_)
                    (+ 1 sum))
                  (set! sum (+ sum 2))))))
            10))

(evaluate '((lambda (x)
              ;; x = 100
              ((lambda (y)
                 ;; y = 101
                 ((lambda (_)
                    (+ x 1))            ; x = 201
                  (set! x (+ y x))))    ; => 202
               (+ x 1)))
            100))

;;; with mutated state
(evaluate '((lambda (n)
              ((lambda (sum)
                 ((lambda (s) (s s n))
                  (lambda (s n)
                    ((lambda (_)
                       (if (= n 0)
                           sum
                           ((lambda (_)
                              (s s n))
                            (set! n (- n 1)))))
                     (set! sum (+ sum n))))))
               0))
            5))

;;; modulo 7
(evaluate '((lambda (s n) (s s n))
            (lambda (s n)
              ((lambda ()
                 ((lambda ()
                    ((lambda (_)
                       (if (= n 0)
                           0
                           (if (> n 0)
                               (s s n)
                               (+ 7 n))))
                     (set! n (- n 7))))))))
            11))

;;; with current continuation
(evaluate '((lambda (n sum gen)
              ((lambda (_)
                 ((lambda (_)
                    ((lambda (_)
                       (if (= n 0) sum (gen gen)))
                     (set! n (- n 1))))
                  (set! sum (+ sum n))))
               (call/cc (lambda (k) (set! gen k)))))
            100 0 '_))

;;; infinite loop
'(evaluate '((lambda (k) (k k)) (call/cc (lambda (k) k))))

(evaluate '((lambda (n sum)
              ((lambda (k)
                 (if (= n 0)
                     sum
                     ((lambda (_)
                        ((lambda (_)
                           (k k))
                         (set! n (- n 1))))
                      (set! sum (+ n sum)))))
               (call/cc (lambda (k) k))))
            100 0))

(evaluate '((lambda (k1)
              (cons 'the
                    (k1 (lambda (k2)
                         (cons 'a
                               (k1 (lambda (k3)
                                    (cons 'b
                                          (k1 (lambda (k4)
                                               '(end)))))))))))
            (call/cc (lambda (k) k))))


