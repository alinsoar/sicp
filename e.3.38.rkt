#lang racket                            ; -*- racket -*-

{require "sicp.rkt"}
{require (only-in racket/base [apply apply/scheme/internal])}

;;;                                        ENVIRONMENT
;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(define internal-scheme-environment
  `((+ . ,+)
    (- . ,-)
    (* . ,*)
    (/ . ,/)
    (= . ,=)
    (eq? . ,eq?)
    (~a . ,~a)
    (display . ,d)))
(define empty-bindings
  '())
(define initial-bindings
  empty-bindings)
(define null-environment
  (lambda (op)
    (op (lambda (name _)    (error "cannot mutate undefined name" name))
        (lambda (name _ __) (error "never happens"))
        (lambda (name _)    (error "variable not defined" name))
        (lambda ()          ""))))
(define environment
  (lambda (parent pairs)
    (define mutate-name!
      (lambda (name newval k)
        (define (iter ps break)
          (cond ((null? ps) (env-mutate! parent name newval k))
                ((eq? name (caar ps))
                 (break
                  (cons (cons name newval)
                        (cdr ps))))
                (else
                 (iter (cdr ps)
                       (lambda (rest)
                         (cons (car ps)
                               rest))))))
        (iter pairs
              (lambda (v)
                (set! pairs v)
                (k 'OK e*self)))))
    (define add-binding!
      (lambda (name val k)
        (if (assoc name pairs)
            (error "already defined" name)
            (set! pairs (cons (cons name val)
                              pairs)))
        (k '__ e*self)))
    (define lookup
      (lambda (sym k)
        (let ((v (assoc sym pairs)))
          (if v
              (k (cdr v) e*self)
              (env-lookup sym parent k)))))
    (define str
      (lambda ()
        (~a pairs ":" (env-str parent))))
    (define e*self
      (lambda (op)
        (op mutate-name!
            add-binding!
            lookup
            str)))
    e*self))
;;; interface
(define env-lookup
  (lambda (name env k)
    (env (lambda (_ __ l ___)
           (l name k)))))
(define env-mutate!
  (lambda (env name newval k)
    (env (lambda (m! .. _ ....)
           (m! name newval k)))))
(define env-add!
  (lambda (env name val k)
    (env (lambda (.. a! _ __)
           (a! name val k)))))
(define env-str
  (lambda (env)
    (env (lambda (_ __ ___ s) (s)))))

;;;                                        EVA LU ATOR
;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(define eval
  (lambda (expr env k)
    '(d "@@ EVAL" expr)
    (cond {(atomic? expr)        (eval-atomic       expr env k)}
          {(variable? expr)      (env-lookup        expr env k)}
          {(define? expr)        (eval-define       expr env k)}
          {(quote? expr)         (eval-quote        expr env k)}
          {(if? expr)            (eval-cond         expr env k)}
          {(lambda? expr)        (eval-lambda       expr env k)}
          {(sequence? expr)      (eval-sequence     expr env k)}
          {(spawn? expr)         (eval-new-thread   expr env k)}
          {(combination? expr)   (eval-combination  expr env k)}
          {else (error "undefined expr" expr)})))

(define apply
  (lambda (op rands e k)
    '(d "@@ APPLY" op)
    (cond ((procedure? op)
           (SCH>> (apply/scheme/internal op rands)
                  e
                  k))
          (else
           (eval (closure-body op)
                 (environment (closure-env op)
                              (map cons (closure-names op) rands))
                 (lambda (v _) 
                   (SCH>> v e k)))))))

(define eval-atomic
  (lambda (expr env k)
    (SCH>> expr env k)))

(define eval-quote
  (lambda (expr env k)
    (SCH>> (quote-val expr) env k)))

(define eval-combination
  (lambda (expr env k)
    ;; (d "["(operator expr) (operands expr) "]")
    (eval (operator expr)
          env
          (lambda (op _)
            ((lambda (s)
               (s s (operands expr)
                  (lambda (rands)
                    (apply op rands env k))))
             (lambda (s rands co)
               (if (null? rands)
                   (co '())
                   (eval (car rands)
                         env
                         (lambda (rand _)
                           (s s
                              (cdr rands)
                              (lambda (rest)
                                (co (cons rand rest)))))))))))))

(define eval-lambda
  (lambda (expr env k)
    ;; (d "LAMBDA:" expr)
    (SCH>> (make-closure expr env) env k)))

(define eval-define
  (lambda (expr env k)
    ;; (d "DEFINE" (operands expr))
    (eval (define-val expr)
          env
          (lambda (v _)
            (env-add! env (define-name expr) v k)))))

(define eval-sequence
  (lambda (expr env k)
    ((lambda (s) (s s env (operands expr)))
     (lambda (s env* seq)
       (eval (car seq)
             env*
             (lambda (v env**)
               (if (null? (cdr seq))
                   (SCH>> v env** k)
                   (SCH>> '_
                          env**
                          (lambda (_ env***)
                            (s s env*** (cdr seq)))))))))))

(define eval-cond
  (lambda (expr env k)
    (eval (if-test expr)
          env
          (lambda (t _)
            (SCH>> '_
                   '_
                   (lambda (_ __)
                     (eval ((if t if-then if-else)
                            expr)
                           env
                           (lambda (v _)
                             (SCH>> v env k)))))))))

(define eval-new-thread
  (lambda (expr env k)
    (d "SPAWN:" (operands expr))
    (SCH+ (car (operands expr)) (make-sequence (cdr (operands expr))) env k)))

;;;                                          SCHEDULER
;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
(define Q
  (lambda ()
    (let ((threads '()))
      (define queue+
        (lambda (th)
          (set! threads (append threads (list th)))))
      (define queue-
        (lambda ()
          (let ((first (car threads)))
            (set! threads (cdr threads))
            first)))
      (define SELF
        (lambda (m)
          (case m
            ('+ queue+)
            ('- queue-)
            ('str (~a (length threads)))
            (else (error "queue" m)))))
      SELF)))
(define THREAD
  (lambda (name k)
    "Each thread has its own identity and a continuation attribute
that changes when the scheduler commutes the execution to other
thread."
    (d "new thread --" name)
    (define v 'none)
    (define e 'none)
    (define update!
      (lambda (k/ v/ e/)
        (set! k k/)
        (set! v v/)
        (set! e e/)))
    (define str
      (lambda ()
        (if (procedure? k)
            (~a name "/" (caddr (regexp-match* #rx"([0-9]+):*" (~a k))))
            name)))
    (lambda (m)
      (case m
        ('id      name)
        ('k       k)
        ('v       v)
        ('e       e)
        ('update! update!)
        ('str     (str))
        ('go      (k v e))))))
(define thread/new
  (lambda (name k)
    ((THREAD name) k)))
(define thread/update!
  (lambda (th k v e)
    ((th 'update!) k v e)))

(define DEFAULT-TICKS 30)

(define scheduler
  (let* ((thread-queue (Q))
         (TICKS DEFAULT-TICKS)
         (main-thread 'void)
         (running-thread 'void))
    (define ticks-reset!
      (lambda ()
        (set! TICKS DEFAULT-TICKS)))
    (define ticks-dec!
      (lambda ()
        (set! TICKS (sub1 TICKS))))
    (define ticks-expired?
      (lambda ()
        (zero? TICKS)))
    
    (define add-thread!
      (lambda (name body env k)
        (define new
          (THREAD
           name
           (lambda _
             (d "starting" name)
             (eval body env
                   (lambda (val _)
                     (d 'thread "#" (new 'id) 'finished "==>" val)
                     (run-next-thread!))))))
        ((thread-queue '+) new)
        (k '_new_thread_ env)))
    (define run-next-thread!
      (lambda ()
        ;; (d ":" (running-thread 'id) "stopped")
        (set! running-thread ((thread-queue '-)))
        ;; (d ":" (running-thread 'id) "restarted")
        (running-thread 'go)))
    (define init*main
      (lambda (main-function)
        (let ((env (environment null-environment
                                (append internal-scheme-environment
                                        initial-bindings)))
              (body (desugar main-function)))
          (set! main-thread (THREAD 'MAIN 'none))
          (set! running-thread main-thread)
          (eval body
                env
                (lambda (val _)
                  (d "**MAIN** finished")
                  val)))))
    (define CONTINUE
      (lambda (v env k)
        (ticks-dec!)
        (if (ticks-expired?)
            (begin (ticks-reset!)
                   (thread/update! running-thread k v env)
                   ((thread-queue '+) running-thread)
                   (run-next-thread!))
            (k v env))))
    (lambda (p)
      "provide permissions to access the scheduler"
      (p CONTINUE add-thread! 'close! init*main))))
(define SCH>>
  (scheduler (lambda (rnt _ __ ___) rnt)))
(define SCH+
  (scheduler (lambda (_ th+ __ ___) th+)))
(define SCH/
  (scheduler (lambda (_ __ close! ___) close!)))
(define SCH!
  (scheduler (lambda (_ __ ___ init/main) init/main)))

;;;                                       TEST THREADS
;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(define TCODE
  '(begin
    
    (define sum
      (lambda (n id)
        ((lambda (s) (s s n (lambda (x) x)))
         (lambda (s n co)
           ;; (display id n)
           (if (= 0 n)
               (co 0)
               (s s
                  (- n 1)
                  (lambda (r)
                    (co (+ r n)))))))))

    (spawn C (sum 10 'C))
    (spawn B (sum 20 'B))
    (spawn A (sum 30 'A))
    
    (define ever
      (lambda (k)
        (if (= k 0)
            'finish
            (ever (- k 1)))))
    
    (ever 2000)))

(define eva.lu.ator
  (lambda (expr)
    (display (~a "==:>" (SCH! expr)))
    (newline)))

;;;                                             SYNTAX
;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(define (tagged? tag e)
  (and (pair? e)
       (eq? tag (operator e))))

(define (operator e)
  (car e))
(define (operands e)
  (cdr e))

(define (atomic? e)
  (or (number? e)
      (string? e)
      (bool? e)
      (closure? e)))

(define (variable? e)
  (symbol? e))

(define make-combination
  (lambda (closure args)
    (list (closure-body closure) args)))
(define (combination? e)
  (list? e))

(define (make-sequence e)
  (cons 'begin e))
(define (sequence? e)
  (tagged? 'begin e))

(define (define? e)
  (tagged? 'define e))
(define (define-name e)
  (and (define? e)
       (cadr e)))
(define (define-val e)
  (and (define? e)
       (caddr e)))

(define (lambda? e)
  (tagged? 'lambda e))
(define (lambda-names e)
  (and (lambda? e)
       (cadr e)))
(define (lambda-body e)
  (and (lambda? e)
       (make-sequence (cddr e))))

(define (make-closure function env)
  (list 'CLOSURE
        (lambda-names function)
        (lambda-body  function)
        env))
(define (closure? e)
  (tagged? 'CLOSURE e))
(define (closure-names e)
  (cadr e))
(define (closure-body e)
  (caddr e))
(define (closure-env e)
  (cadddr e))

(define (quote? e)
  (tagged? 'quote e))
(define (quote-val e)
  (cadr e))

(define (if? e)
  (tagged? 'if e))
(define (if-test e)
  (cadr e))
(define (if-then e)
  (caddr e))
(define (if-else e)
  (cadddr e))

(define (true? e)
  (eq? 'true e))
(define (false? e)
  (eq? 'false e))
(define (bool? e)
  (or (true? e)
      (false? e)))

(define (spawn? e)
  (tagged? 'spawn e))
(define (thread-body e)
  (cadr e))

;;;                                  SYNTAX DESUGARING
;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
(define desugar
  (lambda (expr)
    "Converts the syntactic sugar to kernel syntax."
    '(d "DESUGAR" expr)
    expr
    ;; (cond
    ;;  ((atomic? expr)        (k expr env))
    ;;  ((variable? expr)      (env-lookup env expr k))
    ;;  ((define? expr)        (eval-define expr env k))
    ;;  ((quote? expr)         (k (quote-val expr) env))
    ;;  ((if? expr)            (eval-if expr env k))
    ;;  ((lambda? expr)        (eval-lambda expr env k))
    ;;  ((sequence? expr)      (eval-sequence expr env k))
    ;;  ((combination? expr)   (eval-combination expr env k))
    ;;  (else (error "undefined expr" expr)))
    ))

;;;                                          RUN TESTS
;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(eva.lu.ator TCODE)

(module+ test
  (define make-balance
    (lambda (balance)
      (let ((peter~ false) (paul~ false) (mary~ false))
        (define (peter-read-balance)
          (o "PeterR ")
          (if peter~ peter~ (set! peter~ (+ balance 10))))
        (define (paul-read-balance)
          (o "PaulR  ")
          (if paul~ paul~ (set! paul~ (- balance 20))))
        (define (mary-read-balance)
          (o "MaryR  ")
          (if mary~ mary~ (set! mary~ (- balance (/ balance 2)))))
        (define (peter-set-balance)
          (o "PeterS ")
          (if peter~
              (set! balance peter~)
              (error "Peter should have read the balance before he updates")))
        (define (paul-set-balance)
          (o "PaulS  ")
          (if paul~
              (set! balance paul~)
              (error "Paul should have read the balance before he updates")))
        (define (mary-set-balance)
          (o "MaryS  ")
          (if mary~
              (set! balance mary~)
              (error "Mary should have read the balance before she updates")))
        (define dispatcher
          (lambda (message)
            (cond ((eq? message 'peter-read) peter-read-balance)
                  ((eq? message 'paul-read) paul-read-balance)
                  ((eq? message 'mary-read) mary-read-balance)
                  ((eq? message 'peter-set!) peter-set-balance)
                  ((eq? message 'paul-set!) paul-set-balance)
                  ((eq? message 'mary-set!) mary-set-balance)
                  ((eq? message 'value) balance)
                  (else "Error"))))
        dispatcher)))

  (define (Peter-read balance) ((balance 'peter-read)))
  (define (Paul-read balance) ((balance 'paul-read)))
  (define (Mary-read balance) ((balance 'mary-read)))

  (define (Peter-set! balance) ((balance 'peter-set!)))
  (define (Paul-set! balance) ((balance 'paul-set!)))
  (define (Mary-set! balance) ((balance 'mary-set!)))

  (define PR Peter-read)
  (define pR Paul-read)
  (define MR Mary-read)
  (define PS Peter-set!)
  (define pS Paul-set!)
  (define MS Mary-set!)

  (define seq-peter (list PR PS))
  (define seq-paul (list pR pS))
  (define seq-mary (list MR MS))

  (define make-interleaved-call-combinations
    (lambda ()
      (define set '(a b c))
      (define (is-valid? new comb)
        (< (length (filter (lambda (x) (eq? x new))
                           comb))
           2))
      (define (filter p l)
        (cond ((null? l) '())
              ((p (car l))
               (cons (car l)
                     (filter p (cdr l))))
              (else (filter p (cdr l)))))
      (define (reduce op init l)
        (if (null? l)
            init
            (op (car l)
                (reduce op init (cdr l)))))
      (define (cartesian-product comb)
        (filter
         (lambda (x) x)
         (reduce
          append
          '()
          (map (lambda (x)
                 (map (lambda (y) (if (is-valid? y x)
                                 (cons y x)
                                 false))
                      set))
               comb))))
      (define (make-combinations n co)
        (if (zero? n)
            (co '(()))
            (make-combinations (- n 1)
                               (lambda (x)
                                 (co (cartesian-product x))))))
      (define (replace comb)
        (let ((P (lambda () (cons PR (lambda () (cons PS nil)))))
              (p (lambda () (cons pR (lambda () (cons pS nil)))))
              (M (lambda () (cons MR (lambda () (cons MS nil))))))
          (map (lambda (x)
                 (cond ((eq? x 'a) (let ((v (P))) (set! P (cdr v)) (car v)))
                       ((eq? x 'b) (let ((v (p))) (set! p (cdr v)) (car v)))
                       ((eq? x 'c) (let ((v (M))) (set! M (cdr v)) (car v)))))
               comb)))
      
      (map replace (make-combinations 6 (lambda (x) x)))
      ))

  (define (test-sequential-transaction x1 x2 x3)
    (define balance (make-balance 100))
    (map (lambda (x) (x balance)) (append x1 x2 x3))
    (d "balance value after sequential test:" (balance 'value)))
  (define (test-interleaved-transaction order)
    (define balance (make-balance 100))
    (map (lambda (x) (x balance)) order)
    (d "balance value after interleaved test:" (balance 'value))
    (balance 'value))

  (test-sequential-transaction seq-peter seq-paul  seq-mary)
  (test-sequential-transaction seq-peter seq-mary  seq-paul)
  (test-sequential-transaction seq-paul  seq-peter seq-mary)
  (test-sequential-transaction seq-paul  seq-mary  seq-peter)
  (test-sequential-transaction seq-mary  seq-peter seq-paul)
  (test-sequential-transaction seq-mary  seq-paul  seq-peter)

  "--"

  (map (lambda (order) (test-interleaved-transaction order))
       (make-interleaved-call-combinations))

  )

