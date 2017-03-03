(load "1.1.7.scm")

(define (good-enough? guess x)
  (< (/ (abs (- (improve guess x) guess)) guess) 0.0001))
