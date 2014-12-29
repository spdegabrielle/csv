#lang racket/base
;; For legal info, see file "info.rkt".

(require (planet neil/overeasy:2)
         "csv.rkt")

(test-section 'test-csv

  (define cr (string (integer->char 13)))

  (define lf (string (integer->char 10)))

  (test 'simple-1
        (csv->list (string-append
                    "a" lf "b" lf "c" lf "d" lf ""))
        '(("a") ("b") ("c") ("d")))

  (test 'simple-2
        (csv->list (string-append "  a  "
                                  lf
                                  "  b  "
                                  lf
                                  "  c  "
                                  lf
                                  "  d  "
                                  lf
                                  ""))
        '(("  a  ") ("  b  ") ("  c  ") ("  d  ")))

  (test 'simple-3
        (csv->list (string-append "aaa,bbb,ccc" cr lf
                                  "1,2,3" cr lf))
        '(("aaa" "bbb" "ccc") ("1" "2" "3")))

  (test 'quoted-field
        (csv->list "aaa,\"bbb\",ccc")
        '(("aaa" "bbb" "ccc")))

  (test 'quoted-field-with-comma
        (csv->list "aaa,\"bbb,bbb\",ccc")
        '(("aaa" "bbb,bbb" "ccc")))

  (test 'quoted-field-followed-by-whitespace
        (csv->list "aaa,\"bbb\"   ,ccc")
        '(("aaa" "bbb" "ccc")))

  (test 'quoted-field-with-newline-in-it
        (csv->list (string-append "aaa,\"b" lf "b\",ccc" lf
                                  "ddd,eee,fff" lf))
        `(("aaa" ,(string-append "b" lf "b") "ccc")
          ("ddd" "eee" "fff")))

  (test 'quoted-field-with-doubling-escape-in-middle
        (csv->list "aaa,\"b\"\"b\",ccc")
        '(("aaa" "b\"b" "ccc")))

  (test 'quoted-field-with-doubling-escape-at-beginning
        (csv->list "aaa,\"\"\"bbb\",ccc")
        '(("aaa" "\"bbb" "ccc")))

  (test 'quoted-field-with-doubling-escape-at-end
        (csv->list "aaa,\"bbb\"\"\",ccc")
        '(("aaa" "bbb\"" "ccc")))

  (test 'quoted-field-with-unterminated-quote
        (csv->list "aaa,\"bbb,ccc")
        '(("aaa" "bbb,ccc")))

  (test 'quoted-field-followed-by-eof
        (csv->list "aaa,\"bbb\"")
        '(("aaa" "bbb")))

  (define make-ws-stripping-reader
    (make-csv-reader-maker
     '((strip-leading-whitespace?  . #t)
       (strip-trailing-whitespace? . #t))))

  (test 'whitespace-strip-on-simple-row-terminated-by-eof
        (csv->list (make-ws-stripping-reader
                    "  a  ,  b  ,  c  "))
        '(("a" "b" "c")))

  (define make-nl-adapt-reader
    (make-csv-reader-maker '((newline-type . adapt))))
  
  (test 'try-newline-adapting-reader-maker-first-time
        (csv->list (make-nl-adapt-reader
                    (string-append "aaa,bbb" lf
                                   "ccc" cr ",ddd" cr lf
                                   "eee,fff")))
        `(("aaa" "bbb")
          (,(string-append "ccc" cr)
           ,(string-append "ddd" cr))
          ("eee" "fff")))

  (test 'try-newline-adapting-reader-maker-second-time
        (csv->list (make-nl-adapt-reader
                    (string-append "aaa,bbb" cr lf
                                   "ccc" cr ",ddd" lf cr lf
                                   "eee,fff" cr lf)))
        `(("aaa" "bbb")
          (,(string-append "ccc" cr)
           ,(string-append "ddd" lf))
          ("eee" "fff")))

  (define str
    (string-append "a,b,c"  lf
                   "#d,e,f"  lf
                   "g,h,i"  lf))

  (define make-reader-with-pound-quote
    (make-csv-reader-maker '((comment-chars . (#\#)))))

  (test 'read-str-without-pound-as-comment-char
        (csv->list str)
        '(("a" "b" "c") ("#d" "e" "f") ("g" "h" "i")))

  (test 'read-str-with-pound-as-comment-char
        (csv->list (make-reader-with-pound-quote str))
        '(("a" "b" "c") ("g" "h" "i")))

  (test 'csv->sxml-without-row-and-column-names
        (csv->sxml (string-append "aaa,bbb,ccc" cr lf
                                  "1,2,3" cr lf))
        `(,(string->symbol "*TOP*")
          (row (col-0 "aaa") (col-1 "bbb") (col-2 "ccc"))
          (row (col-0 "1")   (col-1 "2")   (col-2 "3"))))

  (test 'csv->sxml-with-row-and-column-names
        (csv->sxml (string-append "aaa,bbb,ccc" cr lf
                                  "1,2,3" cr lf)
                   'foo
                   '(first second third))
        `(,(string->symbol "*TOP*")
          (foo (first "aaa") (second "bbb") (third "ccc"))
          (foo (first "1")   (second "2")   (third "3"))))

  ;; TODO: Add more test cases.
  )