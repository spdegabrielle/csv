#lang setup/infotab

(define mcfly-planet       'neil/csv:2:0)
(define name               "csv")
(define mcfly-subtitle     "Comma-Separated Value (CSV) Utilities in Racket")
(define blurb              '("Comma-Separated Value (CSV) Utilities"))
(define homepage           "http://www.neilvandyke.org/racket-csv/")
(define mcfly-author       "Neil Van Dyke")
(define repositories       '("4.x"))
(define categories         '(io xml))
(define can-be-loaded-with 'all)
(define primary-file       "main.rkt")
(define scribblings        '(("doc.scrbl" ())))
(define compile-omit-files '("csv.scm"))
(define mcfly-start        "csv.rkt")
(define mcfly-files        '(defaults
                              "csv.rkt"
                              "test-csv.rkt"))
(define mcfly-license      "LGPLv3")

(define mcfly-legal
  "Copyright 2004 -- 2012 Neil Van Dyke.  This program is Free Software; you
can redistribute it and/or modify it under the terms of the GNU Lesser General
Public License as published by the Free Software Foundation; either version 3
of the License, or (at your option) any later version.  This program is
distributed in the hope that it will be useful, but without any warranty;
without even the implied warranty of merchantability or fitness for a
particular purpose.  See http://www.gnu.org/licenses/ for details.  For other
licenses and consulting, please contact the author.")