;;;; copyright 2016 rsiddharth <s@ricketyspace.net>
;;;; under gnu general public license version 3 or higher.

(define-module (net ricketyspace taocp utils mix)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-1)
  #:export (word-to-decimal
            nth-byte))

;;;; Functions that facilitate hacking in MIX

;;; All functions in this module make the following assumptions:
;;; - A byte is 6 bits
;;; - A word is 6 bytes.
;;; - First byte is the sign byte (±)

(define (word-to-decimal word)
  "Converts WORD to a decimal number.

WORD must be a string in this format:

    ±:00:00:00:00:00"
  (let ((bytes (string-split word #\:)))
    (fold (lambda (byte-number d)
            (+ (* (expt (expt 2 6) (- 4 byte-number))
                  (string->number (list-ref bytes byte-number)))
               d))
          0 (iota 5))))
