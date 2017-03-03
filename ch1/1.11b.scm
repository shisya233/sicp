(define (f-iter a b c n)
  (if (= n 3)
    (+ a (* 2 b) (* 3 c))
    (f-iter b c (+ a (* 2 b) (* 3 c)) (- n 1)))
  )
(define (f n)
  (if (< n 3)
    n
    (f-iter 0 1 2 n))
  )

(print (f 0))
(print (f 1))
(print (f 2))
(print (f 3))
(print (f 4))
(print (f 5))

