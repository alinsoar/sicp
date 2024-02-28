#lang racket

(require "sicp.rkt")
(GETMOD 4 1)
(require "e.4.55.S.rkt")

;;; Maintaining the Data Base

(define THE-ASSERTIONS S:empty)

(define (add-assertion! assertion)
  (let ((old-assertions THE-ASSERTIONS))
    (set! THE-ASSERTIONS
          (S:cons assertion old-assertions))
    'ok))

(define (add-assertionX! assertion)
  (set! THE-ASSERTIONS (S:cons assertion THE-ASSERTIONS))
  'ok)

(define (initialize-data-base rules-and-assertions)
  (set! THE-ASSERTIONS (list->S rules-and-assertions)))

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
    (supervisor (Reasoner Louis) (Hacker Alyssa P))))

(module+ test
  
  (initialize-data-base microshaft-data-base)

  (S:display THE-ASSERTIONS)

  (add-assertion! '(NEW-ASSERTION PARAM1 PARAM2))

  (S:display THE-ASSERTIONS)

  (add-assertionX! '(NEW-ASSERTION-X PARAM1-X PARAM2-X))

  (infinite-loop (lambda () (S:display THE-ASSERTIONS))
                 "infinite stream"
                 .0001
                 "LOOP")

  'done)
