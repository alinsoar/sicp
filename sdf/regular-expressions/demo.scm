
;;; An example of looking for lines with r:seq in the regexp.scm file

(pp (r:grep (r:quote "r:seq") "regexp.scm"))
("  (r:seq"
 "    ((0) (r:seq))"
 "(define (r:seq . exprs)"
 "      (apply r:seq"
 "      (r:seq)))"
 "  (apply r:seq"
 "(pp (r:grep (r:seq (r:quote \"a\") (r:dot) (r:quote \"c\")) \"tests.txt\"))"
 " (r:grep (r:seq \" \""
 "    (r:seq (r:bol)")
;Unspecified return value
