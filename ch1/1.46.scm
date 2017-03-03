(load "1.1.7.scm")
(load "fixed-point.scm")
(define (close-enough? v1 v2)
  (< (abs (- v1 v2)) 0.00001))
(define (iterative-improve good-enough? improve)
  (lambda (guess)
    (if (good-enough? guess)
      guess
      ((iterative-improve good-enough? improve) (improve guess)))))
(define (sqrt x)
  ((iterative-improve
     (lambda (guess) (good-enough? guess x))
     (lambda (guess) (improve guess x))) 1.0))
(define (fixed-point f guess)
  ((iterative-improve
     (lambda (x) (close-enough? x (f x)))
     (lambda (x) (f x))) guess))
