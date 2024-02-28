#lang racket

(require "sicp.rkt")
(GETMOD 4 35)

(module+ test
  (amb-test
   '(
     (define (memq a l)
       (cond ((null? l) false)
             ((eq? a (car l)) l)
             (else
              (memq a (cdr l)))))
     
     (define nouns '(noun student professor cat class))

     (define verbs '(verb studies lectures eats sleeps))

     (define articles '(article the a))

     (define prepositions '(prep for to in by with))

     (define (parse-word word-list)
       (require (not (null? *unparsed*)))
       (require (memq (car *unparsed*) (cdr word-list)))
       (let ((found-word (car *unparsed*)))
         (set! *unparsed* (cdr *unparsed*))
         (list (car word-list) found-word)))

     (define *unparsed* '())

     (define (parse input)
       (set! *unparsed* input)
       (let ((sent (parse-sentence)))
         (require (null? *unparsed*))
         sent))

     (define (parse-sentence)
       (list 'sentence
             (parse-noun-phrase)
             (parse-verb-phrase)))

     (define (parse-prepositional-phrase)
       (list 'prep-phrase
             (parse-word prepositions)
             (parse-noun-phrase)))

     (define (parse-verb-phrase)
       (define (maybe-extend verb-phrase)
         (amb verb-phrase
              (maybe-extend (list 'verb-phrase
                                  verb-phrase
                                  (parse-prepositional-phrase)))))
       (maybe-extend (parse-word verbs)))

     (define (parse-simple-noun-phrase)
       (list 'simple-noun-phrase
             (parse-word articles)
             (parse-word nouns)))

     (define (parse-noun-phrase)
       (define (maybe-extend noun-phrase)
         (amb noun-phrase
              (maybe-extend (list 'noun-phrase
                                  noun-phrase
                                  (parse-prepositional-phrase)))))
       (maybe-extend (parse-simple-noun-phrase)))

     ;; 

     (define (parse-sentence0)
       (list 'sentence
             (parse-noun-phrase0)
             (parse-word verbs)))
     
     (define (parse-noun-phrase0)
       (list 'noun-phrase
             (parse-word articles)
             (parse-word nouns)))

     (define (parse0 input)
       (set! *unparsed* input)
       (let ((sent (parse-sentence0)))
         (require (null? *unparsed*))
         sent))

     ))
  (amb-test '("===================="
              (parse0 '(the cat eats))
              try-again
              (parse '(the cat eats))
              try-again
              (parse '(the student with the cat sleeps in the class))
              try-again
              (parse '(the professor lectures to the student with the cat))
              ;; >> (S: [SNP: the professor] {VP: [VP: lectures to the student]
              ;; >>                               [PP: with the cat] })
              try-again
              ;; (S: [SNP: the professor]
              ;;     [VP: lectures {PP: to [NP: [SNP: the student]
              ;;                                [PP: with the cat]]}])
              ;; try-again
              (parse '(the professor lectures to the student in the class with the cat))

;;; (S: THE PROFESSOR (VP: (VP: (VP: lectures      to                  the student) (PP: in      IN THE CLASS))   WITH THE CAT))
;;; (S: THE PROFESSOR      (VP: (VP: lectures      to                  the student) (PP: in (NP: IN THE CLASS     WITH THE CAT))))
;;; (S: THE PROFESSOR      (VP: (VP: lectures (PP: to (NP:       (SNP: the student) (PP: in      IN THE CLASS)))) WITH THE CAT))
;;; (S: THE PROFESSOR           (VP: lectures (PP: to (NP:  (NP: (SNP: the student) (PP: in      IN THE CLASS))   WITH THE CAT))))
;;; (S: THE PROFESSOR           (VP: lectures (PP: to (NP:       (SNP: the student) (PP: in (NP: IN THE CLASS     WITH THE CAT))))))

              try-again                 ;2
              try-again                 ;3
              try-again                 ;4
              try-again                 ;5
              try-again                 ;<
              
              ))
  'done)

