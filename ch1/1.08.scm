(define (cbrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (cbrt-iter (improve guess x)
               x)))
(define (improve guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))
(define (good-enough? guess x)
  (< (/ (abs (- (improve guess x) guess)) guess) 0.0001))
(define (cube x) (* x x x))
(define (cbrt x) (cbrt-iter 1.0 x))
