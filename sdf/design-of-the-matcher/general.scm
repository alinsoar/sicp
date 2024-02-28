

(define (assert p #!optional error-comment irritant)
  (if (not p)
      (begin (if (not (eq? irritant #!default)) (pp irritant))
             (error (if (eq? error-comment #!default)
                        "Failed assertion"
                        error-comment)))))
