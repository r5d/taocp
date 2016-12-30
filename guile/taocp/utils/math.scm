;;;; copyright 2016 rsiddharth <s@ricketyspace.net>
;;;; under gnu general public license version 3 or higher.

(define-module (taocp utils math)
  #:export (compute-exp1-n))


(define (compute-exp1-n fx n)
  "Evaluate function FX for `x` equals 1 to N and return all results
as a list

Expression FX is a function that takes exactly one arugment.

For instance `(lambda (x) (* x x))` is a valid FX.
"
  (cond ((= n 0) '())
        (else (cons (fx n) (compute-exp1-n fx (1- n))))))
