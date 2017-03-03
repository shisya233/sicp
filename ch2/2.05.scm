(define (cons a b) (* (expt 2 a) (expt 3 b)))
(define (car x)
  (if (= (remainder x 2) 0)
    (+ 1 (car (/ x 2)))
    0))
(define (cdr x)
  (if (= (remainder x 3) 0)
    (+ 1 (car (/ x 3)))
    0))

;test
;(define x (cons 3 4))
;(car x)
;;4
;(cdr x)
;;4
