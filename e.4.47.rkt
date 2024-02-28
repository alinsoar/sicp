#lang racket

(require "sicp.rkt")
(GETMOD 4 35)

(require racket/sandbox)

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

     ;; INITIAL DEFINITION
     (define (parse-verb-phrase)
       (define (maybe-extend verb-phrase)
         (amb verb-phrase
              (maybe-extend (list 'verb-phrase
                                  verb-phrase
                                  (parse-prepositional-phrase)))))
       (maybe-extend (parse-word verbs)))
     
     ;; REDEFINED VERB PHRASE -- infinite loop
     (define (parse-verb-phrase)
       (amb (parse-word verbs)
            (list 'verb-phrase
                  (parse-verb-phrase)
                  (parse-prepositional-phrase))))
     ))

  (with-handlers (((lambda (v) (exn? v))
                   (lambda (v)
                     (string-append "run-forever: -- "
                                    (exn-message v))))
                  ((lambda (_) true) (lambda (_) (error "SHOULD NOT HAPPEN"))))
    (call-with-limits .5 1
                      (lambda () 
                        (amb-test '("===================="
                                    (parse '(the cat eats))
                                    try-again
                                    
                                    )))))
  'done)

