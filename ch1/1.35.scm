(load "fixed-point.scm")
(define (fi)
  (fixed-point
    (lambda (x) (+ 1 (/ 1.0 x))) 1.0))
