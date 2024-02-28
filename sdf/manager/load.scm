
(let ((here (directory-pathname (current-load-pathname)))
      (manager-env (make-top-level-environment)))

  (define (load-1 name)
    (load (merge-pathnames name here)
          manager-env))

  (environment-define manager-env
                      'root-directory
                      (directory-pathname
                       (directory-pathname-as-file here)))
  (environment-define manager-env 'manager-env manager-env)

  (load-1 "utils")
  (load-1 "filespec")
  (load-1 "env-model")
  (load-1 "simple-analyzer")
  (load-1 "analyze-sections")
  (load-1 "software-manager")

  (environment-define system-global-environment
                      'manage
                      (access manage-software manager-env)))