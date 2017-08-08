;;;; copyright 2016 rsiddharth <s@ricketyspace.net>
;;;; under gnu general public license version 3 or higher.

(define-module (net ricketyspace taocp one two seven hn)
  #:export (compute-hn
            compute-h2^m
            h2^m-relation))

;;; compute H_n
(define (compute-hn n)
  (cond ((= 1 n) 1.0)
        (else (+ (/ 1.0 n) (compute-hn (1- n))))))

;;; compute H_2^m
(define (compute-h2^m m)
  (let* ((n (expt 2 m)))
    (compute-hn n)))

;;; H_2^m >= 1 + m/2 relation
(define (h2^m-relation m)
  (let ((2^m (expt 2 m))
        (h2^m (compute-h2^m m))
        (rhs (+ 1.0 (/ m 2.0))))
    (list 2^m h2^m rhs)))
