(define (double f)
  (newline) (display "call of double")(lambda (x) (newline) (display x) (f (f x))))
(define inc (lambda (x) (+ x 1)))
(display (((double (double double)) inc) 5))
