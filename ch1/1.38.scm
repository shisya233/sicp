(load "1.37.scm")
(define (cal-e k)
  (+ 2
     (cont-frac
       (lambda (i) 1)
       (lambda (i) (if (= (remainder i 3) 2)
                     (* (+ (/ i 3) 1) 2)
                     1))
       k)))
