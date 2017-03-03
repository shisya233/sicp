(define (f n)
  (if (< n 3)
    n
    (+ (f (- n 3)) (* 2 (f (- n 2))) (* 3 (f (- n 1)))))
 
 )

(print (f 0))
(print (f 1))
(print (f 2))
(print (f 3))
(print (f 4))
(print (f 5))
