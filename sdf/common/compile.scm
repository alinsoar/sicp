
(fluid-let ((sf/default-syntax-table
             (the-environment))
            (sf/default-declarations
             `((usual-integrations ,@overridden-names))))
  (for-each (lambda (path)
              (let ((scm (pathname-new-type path "scm"))
                    (bin (pathname-new-type path "bin"))
                    (com (pathname-new-type path "com")))
                (let ((stime (file-modification-time scm))
                      (btime (file-modification-time bin))
                      (ctime (file-modification-time com)))
                  (cond ((or (not btime) (< btime stime))
                         (cf path))
                        ((or (not ctime) (< ctime btime))
                         (compile-bin-file path))))))
            tests-loaded-pathnames))