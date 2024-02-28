
(set! make-default-dispatch-store
      (lambda ()
        (cache-wrapped-dispatch-store (make-trie-dispatch-store)
                                      implementation-type-name)))