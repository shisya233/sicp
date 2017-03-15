(define (count-pairs x)
  (if (not (pair? x))
    0
    (+ (count-pairs (car x))
       (count-pairs (cdr x))
       1)))

;test
;(define a (cons 1 2))
;(define b (cons 3 4))
;(define x (cons a b))
;(count-pairs x)
;;3
;(set-cdr! a b)
;(count-pairs x)
;;4
;(define y (cons a a))
;(set-cdr! a b)
;(set-car! a b)
;(count-pairs y)
;;7


