;;;; copyright 2016 rsiddharth <s@ricketyspace.net>
;;;; under gnu general public license version 3 or higher.

(define-module (net ricketyspace taocp utils math)
  #:use-module (srfi srfi-1)
  #:export (compute-exp1-n
            fib
            fib-range))

(define (compute-exp1-n fx n)
  "Evaluate function FX for `x` equals 1 to N and return all results
as a list

Expression FX is a function that takes exactly one arugment.

For instance `(lambda (x) (* x x))` is a valid FX.
"
  (cond ((= n 0) '())
        (else (cons (fx n) (compute-exp1-n fx (1- n))))))

(define (fib n)
  "Returns Fibonacci of N.

This function is useless for large values of N. Use `fib-fast`
function defined in (taocp one two eight fib).
"
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))

(define (fib-range start end)
  "Returns Fibonacci sequence between START and END.

This function is useless wher START/END is large. Use `fib-fast-range`
function defined in (taocp one two eight fib).
"
  (let ((f (lambda (x) (cons x (fib x))))
        (count (1+ (- end start ))))
  (map f (iota count start))))
