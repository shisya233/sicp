(load "newton.scm")
(define (cubic a b c)
  (lambda (x)
    (+ (* x x x)
       (* a x x)
       (* b x)
       c)))

(display (newton-method (cubic 0 0 -1.728) 1.0))
