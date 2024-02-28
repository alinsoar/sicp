
;;; nothing to execute -- we miss an interpreter for this language

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

'(data-paths
  (registers ((name N))
             ((name counter)
              (buttons ((name "K<-K+1") (source (operation '1+)))
                       ((name "K<-1") (source (constant 1)))))
             ((name product)
              (buttons ((name "P<-P*K") (source (operation '*)))
                       ((name "P<-1") (source (constant 1))))))

  (operations ((name '>) (inputs (register N)
                                 (register counter)))
              ((name '1+) (inputs (register counter)))
              ((name '*) (inputs (register counter)
                                 (register product)))))

'(controller
  ("K<-1")
  ("P<-1")
  iter
  (test "K>N?")
  (branch (label done))
  ("P<-P*K")
  ("K<-K+1")
  (goto (label iter))
  done)

'nothing-to-execute:missing-interpreter


