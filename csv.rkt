#lang racket/base
;; For legal info, see file "info.rkt".

;(require (planet neil/mcfly))

#;(doc (section "Introduction")

     (para "The "
           (code "csv")
           " package for Racket provides utilities for reading various kinds of
what are commonly known as ``comma-separated value'' (CSV) files.  Since there
is no standard CSV format, this library permits CSV readers to be constructed
from a specification of the peculiarities of a given variant.  A default reader
handles the majority of formats.")

     (para "One of the main uses of this library is to import data from old
crusty legacy applications into Scheme for data conversion and other
processing.  To that end, this library includes various conveniences for
iterating over parsed CSV rows, and for converting CSV input to "
           (hyperlink "http://www.neilvandyke.org/racket-xexp/"
                      "SXML/xexp")
           " format."))

;; This library requires R5RS, SRFI-6, SRFI-23, and an @code{integer->char}
;; procedure that accepts ASCII values.
;;
;; Other implementations of some kind of CSV reading for Scheme dialects include
;; Gauche's @code{text.csv} module, and Scsh's @code{record-reader} and
;; related procedures.  This library intends to be portable and more
;; comprehensive.

;; TODO: In documentation, briefly introduce terms "row", "column", and "field".

(define (%csv:in-arg proc-sym in)
  (cond ((input-port? in) in)
        ((string?     in) (open-input-string in))
        (else (raise-type-error proc-sym "input port or string" in))))

(define (%csv:reader-or-in-arg proc-sym reader-or-in)
  (cond ((procedure?  reader-or-in) reader-or-in)
        ((input-port? reader-or-in) (make-csv-reader reader-or-in))
        ;; TODO: Do we want to close the input-string when done?
        ((string?     reader-or-in) (make-csv-reader (open-input-string
                                                      reader-or-in)))
        (else (raise-type-error proc-sym
                                "csv reader or input port or string"
                                reader-or-in))))

#;(doc (section "Reader Specs")

     (para "CSV readers are constructed using "
           (deftech "reader specs")
           ", which are sets of attribute-value pairs, represented in Scheme as
association lists keyed on symbols.  Each attribute has a default value if not
specified otherwise.  The attributes are:")

     (itemlist

      (item (racketfont "newline-type")
            " --- Symbol representing the newline, or record-terminator,
convention.  The convention can be a fixed character sequence ("
            (racket 'lf)
            ", "
            (racket 'crlf)
            ", or "
            (racket 'cr)
            ", corresponding to combinations of line-feed and carriage-return),
any string of one or more line-feed and carriage-return characters ("
            (racket 'lax)
            "), or adaptive ("
            (racket 'adapt)
            ").  "
            (racket 'adapt)
            " attempts to detect
the newline convention at the start of the input and assume that convention
for the remainder of the input.  Default: "
            (racket 'lax)
            "")

      (item (racketfont "separator-chars")
            " --- Non-null list of characters that serve as field separators.
Normally, this will be a list of one character.  Default: "
            (racket '(#\,))
            " (list of the comma character)")

      (item (racketfont "quote-char")
            " --- Character that should be treated as the quoted field
delimiter character,or "
            (racket #f)
            " if fields cannot be quoted.  Note that there can be only one
quote character.  Default: "
            (racket #\")
            " (double-quote)")

      (item (racketfont "quote-doubling-escapes?")
            " --- Boolean for whether or not a sequence of two "
            (racketfont "quote-char")
            " quote characters within a quoted field constitute an escape
sequence for including a single "
            (racketfont "quote-char")
            " within the string.  Default: "
            (racket #t)
            "")

      (item (racketfont "comment-chars")
            " --- List of characters, possibly null, which comment out the
entire line of input when they appear as the first character in a line.
Default: "
            (racket '())
            " (null list)")

      (item (racketfont "whitespace-chars")
            " --- List of characters, possibly null, that are considered "
            (deftech "whitespace")
            " constituents for purposes of the "
            (racket strip-leading-whitespace?)
            " and "
            (racket strip-trailing-whitespace?)
            " attributes described below. Default: "
            (racket '(#\space))
            " (list of the space character)")

      (item (racketfont "strip-leading-whitespace?")
            " --- Boolean for whether or not leading whitespace in fields
should be stripped.  Note that whitespace within a quoted field is never
stripped.  Default: "
            (racket #f))

      (item (racketfont "strip-trailing-whitespace?")
            " --- Boolean for whether or not trailing whitespace in fields
should be stripped.  Note that whitespace within a quoted field is never
stripped.  Default: "
            (racket #f))

      (item (racketfont "newlines-in-quotes?")
            " --- Boolean for whether or not newline sequences are permitted
within quoted fields.  If true, then the newline characters are included as
part of the field value; if false, then the newline sequence is treated as a
premature record termination.  Default: "
            (racket #t))))

;; TODO: Do not expose this procedure for now.  We expect it to go away and be
;; replaced with two other procedures.
;;
;; @defproc %csv:csv-spec-derive orig-spec changes
;;
;; Yields a new CSV spec that is derived from @var{orig-spec} by applying spec
;; @var{changes} as attribute substitions and additions to the original.  For
;; example, given an original CSV reader spec:
;;
;; @lisp
;; (define my-first-csv-spec
;;   '((newline-type            . lax)
;;     (separator-chars         . (#\,))
;;     (quote-char              . #\")
;;     (quote-doubling-escapes? . #t)
;;     (whitespace-chars        . (#\space))))
;; @end lisp
;;
;; a derived spec with a different @code{separator-chars} attribute and an
;; added @code{comment-chars} attribute can be created like:
;;
;; @lisp
;; (%csv:csv-spec-derive my-first-csv-spec
;;                  '((separator-chars . (#\%))
;;                    (comment-chars   . (#\#))))
;; @result{}
;; ((separator-chars         . (#\%))
;;  (comment-chars           . (#\#))
;;  (newline-type            . lax)
;;  (quote-char              . #\")
;;  (quote-doubling-escapes? . #t)
;;  (whitespace-chars        . (#\space)))
;; @end lisp
;;
;; In that the yielded spec might share some structure with @var{orig-spec}
;; and/or @var{changes}.  Most applications will not use this procedure
;; directly.

(define (%csv:csv-spec-derive orig-spec changes)
  ;; TODO: Make this not share structure.  Error-check and normalize at the
  ;; same time we clone.
  (let ((new-spec '()))
    (let ((add-to-new-spec
           (lambda (alist)
             (for-each (lambda (cell)
                         (or (assq (car cell) new-spec)
                             (set! new-spec (cons cell new-spec))))
                       alist))))
      (add-to-new-spec changes)
      (add-to-new-spec orig-spec)
      (reverse new-spec))))

#;(doc (section "Making Reader Makers")

     (para "CSV readers are procedures that are constructed dynamically to
close over a particular CSV input and yield a parsed row value each time the
procedure is applied.  For efficiency reasons, the reader procedures are
themselves constructed by another procedure, "
           (racket make-csv-reader-maker)
           ", for particular CSV reader specs."))

(define (%csv:csv-error code extra)
  ;; TODO: Maybe make the CSV error handler user-specifiable, or allow user to
  ;; specify some errors that should be disregarded.
  ;;
  ;; TODO: Add position information.  Keep track of character position while
  ;; reading.
  (error '<csv-reader>
         "Erroneous CSV format: ~S"
         (case code
           ((junk-after-quote-close)
            "Junk after close of quoted field:")
           (else (string-append "INTERNAL ERROR: Unknown code: "
                                (symbol->string code))))
         extra))

(define (%csv:newline-check-step0 newline-type c port)
  ;; (display "*DEBUG* (equal? newline-type 'lax) = ")
  ;; (write (equal? newline-type 'lax))
  ;; (newline)
  ;; (display "*DEBUG* (eqv? newline-type 'lax) = ")
  ;; (write (eqv? newline-type 'lax))
  ;; (newline)
  (case newline-type
    ((cr)   (eqv? c #\return))
    ((lf)   (eqv? c #\newline))
    ((crlf) (if (eqv? c #\return)
                (let ((c2 (peek-char port)))
                  (cond ((eof-object? c2)
                         ;; Note: This is a CR-EOF in an input that uses CR-LF
                         ;; for terminating records.  We are discarding the CR,
                         ;; so it will not be added to the field string.  We
                         ;; might want to signal an error.
                         #t)
                        ((eqv? c2 #\newline)
                         (read-char port)
                         #t)
                        (else #f)))
                #f))
    ((lax detect) (cond ((eqv? c #\return)
                         (let ((c2 (peek-char port)))
                           (cond ((eof-object? c2) #t)
                                 ((eqv? c2 #\newline)
                                  (read-char port)
                                  'crlf)
                                 (else 'cr))))
                        ((eqv? c #\newline) 'lf)
                        (else #f)))
    (else (error '%csv:make-portreader/positional
                 "unrecognized newline-type: ~S"
                 newline-type))))

(define %csv:make-portreader/positional
  (letrec-syntax
      ((newline-check
        (syntax-rules ()
          ((_ newline-type c port detected-newline-type)
           ;; Note: "port" and "detected-newline-type" must be identifiers.
           ;; "newline-type" and "c" must be identifiers or self-evals.
           (if (eqv? newline-type 'detect)
               (begin (set! detected-newline-type
                            (%csv:newline-check-step0 newline-type c port))
                      detected-newline-type)
               (%csv:newline-check-step0 newline-type c port)))))
       (gosc-cons
        ;; Note: This is to ensure the output string is gotten and closed
        ;; before consing it with the result of a recursive call.
        (syntax-rules ()
          ((_ os b) (let ((s (get-output-string os))) (cons s b))))))
    (lambda (newline-type
             separator-chars
             quote-char
             quote-doubling-escapes?
             comment-chars
             whitespace-chars
             strip-leading-whitespace?
             strip-trailing-whitespace?
             newlines-in-quotes?)
      (lambda (port)
        (let ((dnlt #f))
          (let read-fields-or-eof ((c (read-char port)))
            (cond
             ((eof-object? c) '())
             ((and strip-leading-whitespace? (memv c whitespace-chars))
              ;; It's leading whitespace char when we're ignoring leading
              ;; whitespace in fields, and there might just be whitespace and
              ;; then an EOF, which should probably be considered just an EOF
              ;; rather than a row with one empty field, so just skip this
              ;; whitespace char.
              (read-fields-or-eof (read-char port)))
             ((and (not (null? comment-chars)) (memv c comment-chars))
              ;; It's a comment char in the first column (or in the first
              ;; non-whitespace column, if "strip-leading-whitespace?" is
              ;; true), so skip to end of line.
              (let ((fake-dnlt #f))
                (let loop ((c (read-char port)))
                  (cond ((eof-object? c) '())
                        ((newline-check newline-type c port fake-dnlt)
                         (read-fields-or-eof (read-char port)))
                        (else (loop (read-char port)))))))
             (else
              ;; It's not going to be just an EOF, so try to read a row.
              (let ((row
                     (let read-fields ((c c))
                       (cond
                        ;; If an EOF or newline in an unquoted field, consider
                        ;; the field and row finished.  (We don't consider EOF
                        ;; before newline to be an error, although perhaps that
                        ;; would be a useful check for a freak premature
                        ;; end-of-input when dealing with "well-formed" CSV).
                        ((or (eof-object? c)
                             (newline-check newline-type c port dnlt))
                         (list ""))
                        ;; If a field separator, finish this field and cons
                        ;; with value of recursive call to get the next field.
                        ((memv c separator-chars)
                         (cons "" (read-fields (read-char port))))
                        ;; If we're ignoring leading whitespace, and it's a
                        ;; whitespace-chars character, then recurse to keep
                        ;; finding the field start.
                        ((and strip-leading-whitespace?
                              (memv c whitespace-chars))
                         (read-fields (read-char port)))
                        ;; If a quote, read a quoted field.
                        ((and quote-char (eqv? c quote-char))
                         (let ((os (open-output-string)))
                           (let loop ((c (read-char port)))
                             (cond
                              ((or (eof-object? c)
                                   (and (not newlines-in-quotes?)
                                        (newline-check newline-type
                                                       c port dnlt)))
                               (list (get-output-string os)))
                              ((and quote-char (eqv? c quote-char))
                               (if quote-doubling-escapes?
                                   (let ((c2 (read-char port)))
                                     (if (eqv? c2 quote-char)
                                         (begin (write-char c2 os)
                                                (loop (read-char port)))
                                         (gosc-cons
                                          os
                                          (let skip-after ((c c2))
                                            (cond
                                             ((or (eof-object? c)
                                                  (newline-check
                                                   newline-type c port dnlt))
                                              '())
                                             ((memv c separator-chars)
                                              (read-fields (read-char port)))
                                             ((memv c whitespace-chars)
                                              ;; Note: We tolerate
                                              ;; whitespace after field
                                              ;; close quote even if
                                              ;; skip-trailing-whitespace?
                                              ;; is false.
                                              (skip-after (read-char port)))
                                             (else (error '%csv:make-portreader/positional
                                                          "Junk after close of quoted field: ~S"
                                                          c)))))))
                                   (gosc-cons os
                                              (read-fields (read-char port)))))
                              (else (write-char c os)
                                    (loop (read-char port)))))))
                        ;; It's the start of an unquoted field.
                        (else
                         (let ((os (open-output-string)))
                           (write-char c os)
                           (let loop ((c (read-char port)))
                             (cond
                              ((or (eof-object? c)
                                   (newline-check newline-type c port dnlt))
                               (list (get-output-string os)))
                              ((memv c separator-chars)
                               (gosc-cons os (read-fields (read-char port))))
                              ((and strip-trailing-whitespace?
                                    (memv c whitespace-chars))
                               ;; TODO: Maybe optimize to avoid creating a new
                               ;; output string every time we see whitespace.
                               ;; We could use a string collector with unwrite.
                               ;; And/or do lookahead to see whether whitespace
                               ;; is only one character.  Do this after we have
                               ;; a better regression test suite.
                               (let ((ws-os (open-output-string)))
                                 (write-char c ws-os)
                                 (let ws-loop ((c (read-char port)))
                                   (cond
                                    ((or (eof-object? c)
                                         (newline-check
                                          newline-type c port dnlt))
                                     (close-output-port ws-os)
                                     (list (get-output-string os)))
                                    ((memv c separator-chars)
                                     (close-output-port ws-os)
                                     (gosc-cons os (read-fields (read-char
                                                                 port))))
                                    ((memv c whitespace-chars)
                                     (write-char c ws-os)
                                     (ws-loop (read-char port)))
                                    (else
                                     (display (get-output-string ws-os) os)
                                     (write-char c os)
                                     (loop (read-char port)))))))
                              (else (write-char c os)
                                    (loop (read-char port)))))))))))
                (if (null? row)
                    row
                    (if (eq? newline-type 'detect)
                        (cons dnlt row)
                        row)))))))))))

(define %csv:make-portreader
  ;; TODO: Make a macro for the three times we list the spec attributes.
  (letrec ((pb (lambda (x) (if x #t #f)))
           (pc (lambda (x)
                 (cond ((char?   x) x)
                       ((string? x) (case (string-length x)
                                      ((1)  (string-ref  x 0))
                                      (else (raise-type-error
                                             'make-csv-reader-maker
                                             "character"
                                             x))))
                       (else (raise-type-error 'make-csv-reader-maker
                                               "character"
                                               x)))))
           (pc-f (lambda (x)
                   (cond ((not     x) x)
                         ((char?   x) x)
                         ((string? x) (case (string-length x)
                                        ((0)  #f)
                                        ((1)  (string-ref  x 0))
                                        (else (raise-type-error
                                               'make-csv-reader-maker
                                               "character or #f"
                                               x))))
                         (else (raise-type-error 'make-csv-reader-maker
                                                 "character or #f"
                                                 x)))))
           (pe (lambda (x acceptable)
                 (if (memq x acceptable)
                     x
                     (raise-type-error
                      'make-csv-reader-maker
                      (let ((os (open-output-string)))
                        (display "symbol from the set " os)
                        (write acceptable os)
                        (get-output-string os))
                      x))))
           (plc-n (lambda (x)
                    (or (list? x)
                        (raise-type-error 'make-csv-reader-maker
                                          "list of characters"
                                          x))
                    (map pc x)))
           (plc (lambda (x)
                  (let ((result (plc-n x)))
                    (if (null? result)
                        (raise-type-error 'make-csv-reader-maker
                                          "non-null list of characters"
                                          x)
                        result)))))
    (lambda (reader-spec)
      (let ((newline-type               'lax)
            (separator-chars            '(#\,))
            (quote-char                 #\")
            (quote-doubling-escapes?    #t)
            (comment-chars              '())
            (whitespace-chars           '(#\space))
            (strip-leading-whitespace?  #f)
            (strip-trailing-whitespace? #f)
            (newlines-in-quotes?        #t))
        ;; TODO: It's erroneous to have two entries for the same attribute in a
        ;; spec.  However, it would be nice if we error-detected duplicate
        ;; entries, or at least had assq semantics (first, rather than last,
        ;; wins).  Use csv-spec-derive's descendants for that.
        (for-each
         (lambda (item)
           (let ((v (cdr item)))
             (case (car item)
               ((newline-type)
                (set! newline-type (pe v '(cr crlf detect lax lf))))
               ((separator-chars)
                (set! separator-chars (plc v)))
               ((quote-char)
                (set! quote-char (pc-f v)))
               ((quote-doubling-escapes?)
                (set! quote-doubling-escapes? (pb v)))
               ((comment-chars)
                (set! comment-chars (plc-n v)))
               ((whitespace-chars)
                (set! whitespace-chars (plc-n v)))
               ((strip-leading-whitespace?)
                (set! strip-leading-whitespace?  (pb v)))
               ((strip-trailing-whitespace?)
                (set! strip-trailing-whitespace? (pb v)))
               ((newlines-in-quotes?)
                (set! newlines-in-quotes? (pb v))))))
         reader-spec)
        (%csv:make-portreader/positional
         newline-type
         separator-chars
         quote-char
         quote-doubling-escapes?
         comment-chars
         whitespace-chars
         strip-leading-whitespace?
         strip-trailing-whitespace?
         newlines-in-quotes?)))))

#;(doc (defproc (make-csv-reader-maker (reader-spec csv-reader-spec?))
         (-> (or/c input-port? string?)
             (-> (listof string?)))

       (para "Constructs a CSV reader constructor procedure from the "
             (racket reader-spec)
             ", with unspecified attributes having their default values.")

       (para "For example, given the input file "
             (filepath "fruits.csv")
             " with the content:")

       (filebox "fruits.csv"
         (verbatim
          "apples  |  2 |  0.42\n"
          "bananas | 20 | 13.69\n"))

       (para "a reader for the file's apparent format can be constructed like:")

       (racketblock
        (define make-food-csv-reader
          (make-csv-reader-maker
           '((separator-chars            #\|)
             (strip-leading-whitespace?  . #t)
             (strip-trailing-whitespace? . #t)))))

       (para "The resulting "
             (racket make-food-csv-reader)
             " procedure accepts one argument, which is either an input port
from which to read, or a string from which to read.  Our example input file
then can be be read by opening an input port on a file and using our new
procedure to construct a reader on it:")

       (racketblock
        (define next-row
          (make-food-csv-reader (open-input-file "fruits.csv"))))

       (para "This reader, "
             (racket next-row)
             ", can then be called repeatedly to yield a parsed representation
of each subsequent row.  The parsed format is a list of strings, one string for
each column.  The null list is yielded to indicate that all rows have already
been yielded.")

       (racketinput (next-row)
                    #,(racketresult ("apples" "2" "0.42")))
       (racketinput (next-row)
                    #,(racketresult ("bananas" "20" "13.69")))
       (racketinput (next-row)
                    #,(racketresult ()))))
(provide make-csv-reader-maker)
(define (make-csv-reader-maker reader-spec)
  (let ((make-portread
         (if (let ((p (assq 'newline-type reader-spec))) (and p (cdr p)))
             ;; Newline-adapting portreader-maker.
             (letrec
                 ((detect-portread
                   (%csv:make-portreader
                    (%csv:csv-spec-derive reader-spec
                                          '((newline-type . detect)))))
                  ;; TODO: The set of cr/crlf/lf newline-type portreaders are
                  ;; constructed optimistically right now for two reasons:
                  ;; 1. we don't yet sanitize reader-specs of shared structure
                  ;; that can be mutated behind our backs; 2. eventually, we
                  ;; want to add a "lots-o-shots?" argument that, when true,
                  ;; would do this anyway.  Consider.
                  (cr-portread
                   (%csv:make-portreader
                    (%csv:csv-spec-derive reader-spec
                                          '((newline-type . cr)))))
                  (crlf-portread
                   (%csv:make-portreader
                    (%csv:csv-spec-derive reader-spec
                                          '((newline-type . crlf)))))
                  (lf-portread
                   (%csv:make-portreader
                    (%csv:csv-spec-derive reader-spec
                                          '((newline-type . lf))))))
               (lambda ()
                 (let ((actual-portread #f))
                   (let ((adapt-portread
                          (lambda (port)
                            (let ((dnlt-row (detect-portread port)))
                              (if (null? dnlt-row)
                                  dnlt-row
                                  (begin (set! actual-portread
                                               (case (car dnlt-row)
                                                 ((cr)   cr-portread)
                                                 ((crlf) crlf-portread)
                                                 ((lf)   lf-portread)
                                                 (else   actual-portread)))
                                         (cdr dnlt-row)))))))
                     (set! actual-portread adapt-portread)
                     (lambda (port) (actual-portread port))))))
             ;; Stateless portreader-maker.
             (let ((reusable-portread
                    (%csv:make-portreader reader-spec)))
               (lambda () reusable-portread)))))
    (lambda (in)
      (let ((port     (%csv:in-arg '<csv-reader> in))
            (portread (make-portread)))
        (lambda () (portread port))))))

#;(doc (section "Making Readers")

     (para "In addition to being constructed from the result of "
           (racket make-csv-reader-maker)
           ", CSV readers can also be constructed using "
           (racket make-csv-reader)
           "."))

#;(doc (defproc (make-csv-reader (in (or/c input-port? string?))
                               (reader-spec csv-reader-spec '()))
         (-> (listof string))

       (para "Construct a CSV reader on the input "
             (racket in)
             ", which is an input port or a string.  If "
             (racket reader-spec)
             " is given, and is not the null list, then a ``one-shot'' reader
constructor is constructed with that spec and used.  If "
             (racket reader-spec)
             " is not given, or is the null list, then the default CSV reader
constructor is used.  For example, the reader from the "
             (racket make-csv-reader-maker)
             " example could alternatively have been constructed like:")

       (racketblock
        (define next-row
          (make-csv-reader
           (open-input-file "fruits.csv")
           '((separator-chars            #\|)
             (strip-leading-whitespace?  . #t)
             (strip-trailing-whitespace? . #t)))))))
(provide make-csv-reader)
(define make-csv-reader
  (let ((default-maker (make-csv-reader-maker '())))
    (lambda (in . rest)
      (let ((spec (cond ((null? rest)       '())
                        ((null? (cdr rest)) (car rest))
                        (else (error 'make-csv-reader
                                     "extraneous arguments: ~S"
                                     (cdr rest))))))
        ((if (null? spec)
             default-maker
             (make-csv-reader-maker spec))
         (%csv:in-arg 'make-csv-reader in))))))

#;(doc (section "High-Level Conveniences")

     (para "Several convenience procedures are provided for iterating over the
CSV rows and for converting the CSV to a list.")

     (para "To the dismay of some Scheme purists, each of these procedures
accepts a "
           (racket reader-or-in)
           " argument, which can be a CSV reader, an input port, or a string.
If not a CSV reader, then the default reader constructor is used.  For example,
all three of the following are equivalent:")

     (racketblock
      (csv->list                                     #,(italic "string")  )

      (csv->list (make-csv-reader                    #,(italic "string") ))

      (csv->list (make-csv-reader (open-input-string #,(italic "string"))))))

#;(doc (defproc (csv-for-each (proc (-> (listof string?) any))
                            (reader-or-in (or/c (-> (listof string?))
                                                input-port?
                                                string?)))
         any)
     (para "Similar to Racket's "
           (racket for-each)
           ", applies "
           (racket proc)
           ", a procedure of one argument, to each parsed CSV row in series. "
           (racket reader-or-in)
           " is the CSV reader, input port, or string.  The return value is
undefined.")
     ;; TODO: Doc an example for this.
     )
(provide csv-for-each)
(define (csv-for-each proc reader-or-in)
  (let ((reader (%csv:reader-or-in-arg "csv-for-each" reader-or-in)))
    (let loop ((row (reader)))
      (or (null? row)
          (begin (proc row)
                 (loop (reader)))))))

#;(doc (defproc (csv-map (proc (-> (listof string?) any/c))
                       (reader-or-in (or/c (-> (listof string?))
                                           input-port?
                                           string?)))
         any/c
       (para "Similar to Racket's "
             (racket map)
             ", applies "
             (racket proc)
             ", a procedure of one argument, to each parsed CSV row in series,
and yields a list of the values of each application of "
             (racket proc)
             ", in order. "
             (racket reader-or-in)
             " is the CSV reader, input port, or string.")
       ;; TODO: Doc an example for this.
       ))
(provide csv-map)
(define (csv-map proc reader-or-in)
  (let ((reader (%csv:reader-or-in-arg "csv-for-each" reader-or-in)))
    (let loop ((row (reader)) (ret null))
      (if (null? row)
          (reverse ret)
          (let ((ret (cons (proc row) ret)))
            (loop (reader) ret))))))

;; (define (csv-map proc reader-or-in)
;;   (let ((reader (%csv:reader-or-in-arg "csv-for-each" reader-or-in)))
;;     (let ((head '()))
;;       (let ((row (reader)))
;;         (if (null? row)
;;             head
;;             (let ((pair (cons (proc row) '())))
;;               (set! head pair)
;;               (let loop ((prior pair))
;;                 (let ((row (reader)))
;;                   (if (null? row)
;;                       head
;;                       (let ((pair (cons (proc row) '())))
;;                         (set-cdr! prior pair)
;;                         (loop pair)))))))))))

#;(doc (defproc (csv->list (reader-or-in (or/c (-> (listof string?))
                                             input-port?
                                             string?)))
         (listof (listof string?))
       (para "Yields a list of CSV row lists from input "
             (racket reader-or-in)
             ", which is a CSV reader, input port, or string.")
       ;; TODO: Doc an example for this.
       ))
(provide csv->list)
(define (csv->list reader-or-in)
  (csv-map values reader-or-in))

;; (define (csv->list reader-or-in)
;;   (let ((reader (%csv:reader-or-in-arg "csv->list" reader-or-in)))
;;     (let ((head '()))
;;       (let ((row (reader)))
;;         (if (null? row)
;;             head
;;             (let ((pair (cons row '())))
;;               (set! head pair)
;;               (let loop ((prior pair))
;;                 (let ((row (reader)))
;;                   (if (null? row)
;;                       head
;;                       (let ((pair (cons row '())))
;;                         (set-cdr! prior pair)
;;                         (loop pair)))))))))))

#;(doc (section "Converting CSV to SXML/xexp")

     (para "The "
           (racket csv->sxml)
           " procedure can be used to convert CSV to SXML/xexp format,
for processing with various XML tools."))

#;(doc (defproc (csv->sxml (reader-or-in (or/c (-> (listof string?))
                                             input-port?
                                             string?))
                         (row-element symbol? 'row)
                         (col-elements (listof symbol?) '()))
         sxml/xexp

       (para "Reads CSV from input "
             (racket reader-or-in)
             " (which is a CSV reader, input port, or string), and yields an
SXML/xexp representation.  If given, "
             (racket row-element)
             " is a symbol for the XML row element.  If "
             (racket row-element)
             " is not given, the default is the symbol "
             (racket row)
             ".  If given "
             (racket col-elements)
             " is a list of symbols for the XML column elements.  If not given,
or there are more columns in a row than given symbols, column element symbols
are of the format ``"
             (code "col-")(italic "n")
             "'' where "
             (italic "n")
             " is the column number (the first column being number 0, not 1).")

       (para "For example, given a CSV-format file "
             (filepath "friends.csv")
             " that has the contents:")
       
       (filebox "friends.csv"
         (verbatim "Binoche,Ste. Brune,33-1-2-3\n"
                   "Posey,Main St.,555-5309\n"
                   "Ryder,Cellblock 9,\n"))
       
       (para "with elements not given, the result is:")
       
       (racketinput
        (csv->sxml (open-input-file "friends.csv")))
       (racketresultblock
        (*TOP* (row (col-0 "Binoche") (col-1 "Ste. Brune")  (col-2 "33-1-2-3"))
               (row (col-0 "Posey")   (col-1 "Main St.")    (col-2 "555-5309"))
               (row (col-0 "Ryder")   (col-1 "Cellblock 9") (col-2 ""))))
       
       (para "With elements given, the result is like:")
       
       (racketinput
        (csv->sxml (open-input-file "friends.csv")
            'friend
            '(name address phone)))
       (racketresultblock
        (*TOP* (friend (name    "Binoche")
                       (address "Ste. Brune")
                       (phone   "33-1-2-3"))
               (friend (name    "Posey")
                       (address "Main St.")
                       (phone   "555-5309"))
               (friend (name    "Ryder")
                       (address "Cellblock 9")
                       (phone   ""))))))
(provide csv->sxml)
(define csv->sxml
  (let* ((top-symbol
          (string->symbol "*TOP*"))
         (make-col-symbol
          (lambda (n)
            (string->symbol (string-append "col-" (number->string n)))))
         (default-col-elements
           (let loop ((i 0))
             (if (= i 32) ; arbitrary magic number
                 '()
                 (cons (make-col-symbol i) (loop (+ 1 i)))))))
    ;; TODO: Have option to error when columns count doesn't match provided
    ;; column name list.
    (lambda (reader-or-in . rest)
      (let ((reader       (%csv:reader-or-in-arg "csv->sxml"
                                                 reader-or-in))
            (row-element  'row)
            (col-elements #f))
        ;; TODO: Maybe use case-lambda.
        (or (null? rest)
            (begin (set! row-element (car rest))
                   (let ((rest (cdr rest)))
                     (or (null? rest)
                         (begin (set! col-elements (car rest))
                                (let ((rest (cdr rest)))
                                  (or (null? rest)
                                      (error 'csv->sxml
                                             "extraneous arguments: ~S"
                                             rest))))))))
        ;; TODO: We could clone and grow default-col-elements for the duration
        ;; of this procedure.
        (cons top-symbol
              (csv-map (lambda (row)
                         (cons row-element
                               (let loop ((vals  row)
                                          (i     0)
                                          (names (or col-elements
                                                     default-col-elements)))
                                 (if (null? vals)
                                     '()
                                     (cons (list (if (null? names)
                                                     (make-col-symbol i)
                                                     (car names))
                                                 (car vals))
                                           (loop (cdr vals)
                                                 (+ 1 i)
                                                 (if (null? names)
                                                     '()
                                                     (cdr names))))))))
                       reader))))))

;; TODO: Make a define-csv-reader/positional, for great constant-folding.
;; That's part of the reason some things are done the way they are.

;; TODO: Make a csv-bind, as a newbie convenience for people without advanced
;; match forms, which looks good in examples.  This is better than a
;; csv-map/bind and a csv-for-each/bind.
;;
;; (csv-for-each/bind ((column-binding ...) body ...)
;;               { (else => closure) | (else body ...) | }
;;               input-port
;;               [ csv-reader ])
;;
;; (csv-for-each/bind
;;  ((lastname firstname email)
;;   ...)
;;  (else => (lambda (row) (error "CSV row didn't match pattern" row)))
;;  my-input-port
;;  my-csv-reader)

;; TODO: Handle escapes, once we find an actual example or specification of any
;; flavor of escapes in CSV other than quote-doubling inside a quoted field.

;; TODO: Add a spec attribute for treating adjacent separators as one, or
;; skipping empty fields.  This would probably only be used in practice for
;; parsing whitespace-separated input.

;; TODO: Get access to MS Excel or documentation, and make this correct.
;;
;; (define msexcel-csv-reader-spec
;;   '((newline-type               . crlf)
;;     (separator-chars            . (#\,))
;;     (quote-char                 . #\")
;;     (quote-doubling-escapes?    . #t)
;;     (comment-chars              . ())
;;     (whitespace-chars           . (#\space))
;;     (strip-leading-whitespace?  . #f)
;;     (strip-trailing-whitespace? . #f)
;;     (newlines-in-quotes?        . #t)))

;; TODO: Maybe put this back in.
;;
;; (define default-csv-reader-spec
;;   '((newline-type               . lax)
;;     (separator-chars            . (#\,))
;;     (quote-char                 . #\")
;;     (quote-doubling-escapes?    . #t)
;;     (comment-chars              . ())
;;     (whitespace-chars           . (#\space))
;;     (strip-leading-whitespace?  . #f)
;;     (strip-trailing-whitespace? . #f)
;;     (newlines-in-quotes?        . #t)))

;; TODO: Implement CSV writing, after CSV reading is field-tested and polished.

;; TODO: Call "close-input-port" once eof-object is hit, but make sure we still
;; can return an empty list on subsequent calls to the CSV reader.

;; TODO: Consider switching back to returning eof-object at the end of input.
;; We originally changed to returning the null list because we might want to
;; synthesize the EOF, and there is no R5RS binding for the eof-object.

;; TODO: [2005-12-09] In one test, Guile has a stack overflow when parsing a
;; row with 425 columns.  Wouldn't hurt to see if we can make things more
;; tail-recursive.

#;(doc history

     (#:planet 2:0 #:date "2012-06-13"
               (itemlist
                (item "Converted to McFly and Overeasy.")))

     (#:version "0.11" #:date "2011-08-22" #:planet 1:7
                "Changed URL.  Changed references to Scheme to Racket.  A
little code cleanup, including using Racket "
                (racket error)
                " better.")

     (#:version "0.10" #:date "2010-04-13" #:planet 1:6
                "Documentation fix.")

     (#:version "0.9" #:date "2009-03-14" #:planet 1:5
                "Documentation fix.")

     (#:version "0.8" #:date "2009-02-23" #:planet 1:4
                "Documentation changes.")

     (#:version "0.7" #:date "2009-02-22" #:planet 1:3
                "License is now LGPL 3.  Moved to author's new Scheme administration system.")

     (#:version "0.6" #:date "2008-08-12" #:planet 1:2
                "For PLT 4 compatibility, new versions of "
                (racket csv-map)
                " and "
                (racket csv->list)
                " that don't use "
                (racket set-cdr!)
                " (courtesy of Doug Orleans). PLT 4 "
                (racket if)
                " compatibility change.  Minor documentation fixes.")

     (#:version "0.5" #:date "2005-12-09"
                "Changed a non-R5RS use of "
                (racket letrec)
                " to "
                (racket let*)
                ".  Thanks to David Pirotte and Guile.")

     (#:version "0.4" #:date " 2005-06-07"
                "Converted to Testeez.  Minor documentation changes.")

     (#:version "0.3" #:date " 2004-07-21"
                "Minor documentation changes.  Test suite now disabled by default.")

     (#:version "0.2" #:date " 2004-06-01"
                "Work-around for "
                (racket case)
                "-related bug in Gauche 0.8 and 0.7.4.2 that was tickled by "
                (racket csv-internal:make-portreader/positional)
                ".  Thanks to Grzegorz Chrupala for reporting.")

     (#:version "0.1" #:date " 2004-05-31"
                "First release, for testing with real-world input."))