
(define __display_generic
  (lambda (out close)
    (lambda args
      (define (iter a co)
        (if (null? a)
            (co (lambda ()
                  (close)))
            (iter (cdr a)
                  (lambda (x)
                    (co (lambda ()
                          (out (car a))
                          (x)))))))
      (iter args (lambda (x) (x) ) ) ) ) )
(define __d
  (__display_generic (lambda (x) (display " ") (display x))
                     (lambda () (newline))))

;;; DATABASES

(define microshaft-data-base
  '(
    ;; from section 4.4.1
    (rule (address (Bitdiddle Ben) (Slumerville (Ridge Road) 10)))
    (rule (job (Bitdiddle Ben) (computer wizard)))
    (rule (salary (Bitdiddle Ben) 60000))

    (rule (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78)))
    (rule (job (Hacker Alyssa P) (computer programmer)))
    (rule (salary (Hacker Alyssa P) 40000))
    (rule (supervisor (Hacker Alyssa P) (Bitdiddle Ben)))

    (rule (address (Fect Cy D) (Cambridge (Ames Street) 3)))
    (rule (job (Fect Cy D) (computer programmer)))
    (rule (salary (Fect Cy D) 35000))
    (rule (supervisor (Fect Cy D) (Bitdiddle Ben)))

    (rule (address (Tweakit Lem E) (Boston (Bay State Road) 22)))
    (rule (job (Tweakit Lem E) (computer technician)))
    (rule (salary (Tweakit Lem E) 25000))
    (rule (supervisor (Tweakit Lem E) (Bitdiddle Ben)))

    (rule (address (Reasoner Louis) (Slumerville (Pine Tree Road) 80)))
    (rule (job (Reasoner Louis) (computer programmer trainee)))
    (rule (salary (Reasoner Louis) 30000))
    (rule (supervisor (Reasoner Louis) (Hacker Alyssa P)))

    (rule (supervisor (Bitdiddle Ben) (Warbucks Oliver)))

    (rule (address (Warbucks Oliver) (Swellesley (Top Heap Road))))
    (rule (job (Warbucks Oliver) (administration big wheel)))
    (rule (salary (Warbucks Oliver) 150000))

    (rule (address (Scrooge Eben) (Weston (Shady Lane) 10)))
    (rule (job (Scrooge Eben) (accounting chief accountant)))
    (rule (salary (Scrooge Eben) 75000))
    (rule (supervisor (Scrooge Eben) (Warbucks Oliver)))

    (rule (address (Cratchet Robert) (Allston (N Harvard Street) 16)))
    (rule (job (Cratchet Robert) (accounting scrivener)))
    (rule (salary (Cratchet Robert) 18000))
    (rule (supervisor (Cratchet Robert) (Scrooge Eben)))

    (rule (address (Aull DeWitt) (Slumerville (Onion Square) 5)))
    (rule (job (Aull DeWitt) (administration secretary)))
    (rule (salary (Aull DeWitt) 25000))
    (rule (supervisor (Aull DeWitt) (Warbucks Oliver)))

    (rule (can-do-job (computer wizard) (computer programmer)))
    (rule (can-do-job (computer wizard) (computer technician)))

    (rule (can-do-job (computer programmer)
                      (computer programmer trainee)))

    (rule (can-do-job (administration secretary)
                      (administration big wheel)))

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
  '( (rule (Greek Socrates))
     (rule (Greek Plato))
     (rule (Greek Aristote))
     (rule (Greek Zeus)) 
     (rule (god Zeus))

     (rule (mortal ?x) (human ?x))
     (rule (fallible ?x) (human ?x))

     (rule (human ?x)
           (and (Greek ?x) 
                (not (god ?x))))
     
     (rule (address ?x Olympus)
           (and (Greek ?x) (god ?x)))))

(define peano-data-base
  '((rule (add zero ?N ?N))
    (rule (add (S ?A) ?B (S ?R))
          (add ?A ?B ?R)
          (check-predicate (?A) test123)
          )))

(define a-data-base
  '((rule (append () ?y ?y))
    (rule (append (?u . ?v) ?y (?u . ?z))
          (append ?v ?y ?z))))

(define t-data-base
  '(
    (rule (testunion ?l)
          (append (union . ?x) ((union . ?y) . ?z) ?l)
          (check-predicate (?x ?y ?z ?l) test123))
    (rule (~testunion ?l)
          (= ?l (union . ?r))
          (not (append ?x1 ((union . ?x2) . ?x3) ?r))
          (check-predicate (?r) test123))))

(define test
  (lambda ()
    ;; (initialize-data-base greeks-data-base)
    ;; (initialize-data-base a-data-base)
    ;; (initialize-data-base t-data-base)
    (initialize-data-base peano-data-base)
    ;; (initialize-data-base microshaft-data-base)
    
    '(query-driver-loop '(
                         (mortal ?x)
                         
                         ))
    
    '(query-driver-loop
      '((supervisor ?x (Bitdiddle Ben))
        (job ?name (accounting . ?job))
        (address ?name (Slumerville . ?address))))

    '(query-driver-loop '((same ?x ?x)))

    '(query-driver-loop
      '((lives-near ?x ?y)
        (COMM "ORDER 1")
        (and (supervisor ?x ?y)
             (not (job ?x (computer programmer))))
        (COMM "REVERSED ORDER")
        (and (not (job ?x (computer programmer)))
             (supervisor ?x ?y))
        (and (and (supervisor ?x ?y)
                  (not (job ?x (computer programmer))))
             (and (not (job ?x (computer programmer)))
                  (supervisor ?x ?y)))))

    (query-driver-loop
     '(
       (add zero zero ?x)
       (add (S zero) (S zero) ?x)
       (add (S (S zero)) (S zero) ?x)
       (add ?x ?y (S (S (S (S (S zero))))))
       ))
    
    '(query-driver-loop 
     '((append (a b) (c d) ?r)
       (append (a b) ?w (a b c d))
       (append ?x ?y (a b c d))))
    
    '(query-driver-loop 
     '( 
       
       ;; (testunion (union (union m n p) b d))
       ;; (testunion (union b (union m (n) p) d))
       ;; (testunion (union b d (union m n p)))
       ;; (testunion (union b d (unionx (m) n p)
       ;;                   (unionx m n p)))
       
       (~testunion (union b unionx m n p d))
       ;; (~testunion (union b (unionx) m n p d))
       ;; (~testunion (union (union) b m n p d))
       ;; (~testunion (union b (union) m n p d))
       ;; (~testunion (unionx b (union a) m n p d))
       ;; (~testunion (union b m n p d (union x)))
       
       ))
    
    
    'done))

