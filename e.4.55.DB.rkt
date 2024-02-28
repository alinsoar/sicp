#lang racket

(require "sicp.rkt")
(GETMOD 4 1)
(require "e.4.55.S.rkt")

;;; Maintaining the Data Base

(define DB
  (let ((THE-ASSERTIONS S:empty)
        (THE-RULES S:empty))

    ;; From instructor's manual -- TABLE OPERATIONS

    (define get      (lambda () (error)))
    (define put      (lambda () (error)))
    (define db-table (lambda () (error)))
    (define (make-table)
      (let ((local-table (list)))
        (define (lookup key-1 key-2)
          (let ((subtable (assoc key-1 local-table)))
            (if subtable
                (let ((record (assoc key-2 (cdr subtable))))
                  (if record
                      (cdr record)
                      false))
                false)))
        (define (insert! key-1 key-2 value)
          '(d "INSERT" (~a key-1 #:width 15) (~a key-2 #:width 20) "VAL")
          (let ((subtable (cdr (or (assoc key-1 local-table)
                                   '(empty-subtable)))))
            (let ((updated-subtable (cons (cons key-2 value)
                                          subtable)))
              (set! local-table (cons (cons key-1 updated-subtable)
                                      local-table))))
          local-table)
        (define (dispatch m)
          (cond ((eq? m 'lookup-proc) lookup)
                ((eq? m 'insert-proc!) insert!)
                ((eq? m 'get-table) local-table)
                (else (error "Unknown operation -- TABLE" m))))
        dispatch))
    
    (define (fetch-assertions pattern frame)
      (if (use-index? pattern)
          (get-indexed-assertions pattern)
          (get-all-assertions)))
    (define (get-all-assertions) THE-ASSERTIONS)
    (define (get-indexed-assertions pattern)
      (get-stream (index-key-of pattern) 'assertion-stream))
    (define (get-stream key1 key2)
      (or (get key1 key2) S:empty))

    (define (fetch-rules pattern frame)
      (if (use-index? pattern)
          (get-indexed-rules pattern)
          (get-all-rules)))
    (define (get-all-rules) THE-RULES)
    (define (get-indexed-rules pattern)
      (S:append
       (get-stream (index-key-of pattern) 'rule-stream)
       (get-stream '? 'rule-stream)))
    (define (add-rule-or-assertion! assertion)
      (if (rule? assertion)
          (add-rule! assertion)
          (add-assertion! assertion)))
    (define (add-assertion! assertion)
      (store-assertion-in-index assertion)
      (let ((old-assertions THE-ASSERTIONS))
        (set! THE-ASSERTIONS
              (S:cons assertion old-assertions))
        'ok))
    (define (add-rule! rule)
      (store-rule-in-index rule)
      (let ((old-rules THE-RULES))
        (set! THE-RULES (S:cons rule old-rules))
        'ok))
    (define (store-assertion-in-index assertion)
      (and (indexable? assertion)
           (let ((key (index-key-of assertion)))
             (let ((current-assertion-stream
                    (get-stream key 'assertion-stream)))
               (put key
                    'assertion-stream
                    (S:cons assertion
                            current-assertion-stream))))))
    (define (store-rule-in-index rule)
      (let ((pattern (conclusion rule)))
        (and (indexable? pattern)
             (let ((key (index-key-of pattern)))
               (let ((current-rule-stream
                      (get-stream key 'rule-stream)))
                 (put key
                      'rule-stream
                      (S:cons rule current-rule-stream)))))))
    (define (indexable? pat)
      (or (constant-symbol? (car pat))
          (var? (car pat))))
    (define (index-key-of pat)
      (let ((key (car pat)))
        (if (var? key) '? key)))
    (define (use-index? pat)
      (constant-symbol? (car pat)))

    (define (initialize-data-base rules-and-assertions)
      (define (deal-out r-and-a rules assertions)
        (cond ((null? r-and-a)
               (set! THE-ASSERTIONS (list->S assertions))
               (set! THE-RULES (list->S rules))
               'done)
              (else
               (let ((s (query-syntax-process (car r-and-a))))
                 (cond ((rule? s)
                        (store-rule-in-index s)
                        (deal-out (cdr r-and-a)
                                  (cons s rules)
                                  assertions))
                       (else
                        (store-assertion-in-index s)
                        (deal-out (cdr r-and-a)
                                  rules
                                  (cons s assertions))))))))
      (let ((operation-table (make-table)))
        (set! get (operation-table 'lookup-proc))
        (set! put (operation-table 'insert-proc!))
        (set! db-table operation-table))
      (deal-out rules-and-assertions '() '()))
    
    (lambda (m)
      (cond ((eq? m 'fetch-assertions)
             fetch-assertions)
            ((eq? m 'fetch-rules)
             fetch-rules)
            ((eq? m 'initialize-data-base)
             initialize-data-base)
            ((eq? m 'add-rule-or-assertion!)
             add-rule-or-assertion!) ) ) ) )

(define initialize-data-base
  (DB 'initialize-data-base))

(define (rule? statement)
  (tagged-list? statement 'rule))
(define (conclusion rule)
  (cadr rule))
(define (rule-body rule)
  (if (null? (cddr rule))
      '(always-true)
      (caddr rule)))

(define (query-syntax-process exp)
  (map-over-symbols expand-question-mark exp))
(define (map-over-symbols proc exp)
  (cond ((pair? exp)
         (cons (map-over-symbols proc (car exp))
               (map-over-symbols proc (cdr exp))))
        ((symbol? exp) (proc exp))
        (else exp)))
(define (expand-question-mark symbol)
  (let ((chars (symbol->string symbol)))
    (if (string=? (substring chars 0 1) "?")
        (list '?
              (string->symbol
               (substring chars 1 (string-length chars))))
        symbol)))
(define (contract-question-mark variable)
  (string->symbol
   (string-append "?"
                  (if (number? (cadr variable))
                      (string-append (symbol->string (caddr variable))
                                     "-"
                                     (number->string (cadr variable)))
                      (symbol->string (cadr variable))))))

(define (var? exp)
  (tagged-list? exp '?))
(define (constant-symbol? exp)
  (symbol? exp))

;;; DATABASES

(define microshaft-data-base
  '(
    ;; from section 4.4.1
    (address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))
    (job (Bitdiddle Ben) (computer wizard))
    (salary (Bitdiddle Ben) 60000)

    (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78))
    (job (Hacker Alyssa P) (computer programmer))
    (salary (Hacker Alyssa P) 40000)
    (supervisor (Hacker Alyssa P) (Bitdiddle Ben))

    (address (Fect Cy D) (Cambridge (Ames Street) 3))
    (job (Fect Cy D) (computer programmer))
    (salary (Fect Cy D) 35000)
    (supervisor (Fect Cy D) (Bitdiddle Ben))

    (address (Tweakit Lem E) (Boston (Bay State Road) 22))
    (job (Tweakit Lem E) (computer technician))
    (salary (Tweakit Lem E) 25000)
    (supervisor (Tweakit Lem E) (Bitdiddle Ben))

    (address (Reasoner Louis) (Slumerville (Pine Tree Road) 80))
    (job (Reasoner Louis) (computer programmer trainee))
    (salary (Reasoner Louis) 30000)
    (supervisor (Reasoner Louis) (Hacker Alyssa P))

    (supervisor (Bitdiddle Ben) (Warbucks Oliver))

    (address (Warbucks Oliver) (Swellesley (Top Heap Road)))
    (job (Warbucks Oliver) (administration big wheel))
    (salary (Warbucks Oliver) 150000)

    (address (Scrooge Eben) (Weston (Shady Lane) 10))
    (job (Scrooge Eben) (accounting chief accountant))
    (salary (Scrooge Eben) 75000)
    (supervisor (Scrooge Eben) (Warbucks Oliver))

    (address (Cratchet Robert) (Allston (N Harvard Street) 16))
    (job (Cratchet Robert) (accounting scrivener))
    (salary (Cratchet Robert) 18000)
    (supervisor (Cratchet Robert) (Scrooge Eben))

    (address (Aull DeWitt) (Slumerville (Onion Square) 5))
    (job (Aull DeWitt) (administration secretary))
    (salary (Aull DeWitt) 25000)
    (supervisor (Aull DeWitt) (Warbucks Oliver))

    (can-do-job (computer wizard) (computer programmer))
    (can-do-job (computer wizard) (computer technician))

    (can-do-job (computer programmer)
                (computer programmer trainee))

    (can-do-job (administration secretary)
                (administration big wheel))

    (rule (lives-near ?person-1 ?person-2)
          (and (address ?person-1 (?town . ?rest-1))
               (address ?person-2 (?town . ?rest-2))
               (not (same ?person-1 ?person-2))))

    (rule (same ?x ?x))

    (rule (wheel ?person)
          (and (supervisor ?middle-manager ?person)
               (supervisor ?x ?middle-manager)))

    (rule (outranked-by ?staff-person ?boss)
          (or (supervisor ?staff-person ?boss)
              (and (supervisor ?staff-person ?middle-manager)
                   (outranked-by ?middle-manager ?boss))))))

(define a-data-base
  '((rule (append () ?y ?y))
    (rule (append (?u . ?v) ?y (?u . ?z))
          (append ?v ?y ?z))))

(define genesis-data-base
  '( (son Adam Cain)
     (son Cain Enoch)
     (son Enoch Irad)
     (son Irad Mehujael)
     (son Mehujael Methushael)
     (son Methushael Lamech)
     (wife Lamech Ada)
     (son Ada Jabal)
     (rule (grandson  ?S ?G)
           (and (son ?S ?F)
                (son ?F ?G)))
     
     (rule (son       ?M ?S)
           (and (wife ?M ?W)
                (son  ?W ?S)))))

(define mickey-data-base
  '((married Minnie Mickey) 
    (rule (married ?x ?y) 
          (married ?y ?x))))

(define greeks-data-base
  '( (Greek Socrates) (Greek Plato)
     (Greek Zeus)     (god Zeus)

     (rule (mortal ?x) (human ?x))
     (rule (fallible ?x) (human ?x))

     (rule (human ?x)
           (and (Greek ?x) (not (god ?x))))
     
     (rule (address ?x Olympus)
           (and (Greek ?x) (god ?x)))))

(define peano-data-base
  '((rule (add zero ?N ?N))
    (rule (add (S ?A) ?B (S ?R))
          (add ?A ?B ?R))))

(provide microshaft-data-base
         a-data-base
         genesis-data-base
         mickey-data-base
         peano-data-base

         initialize-data-base
         DB
         var?
         constant-symbol?
         conclusion
         rule-body
         query-syntax-process
         contract-question-mark)


