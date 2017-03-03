;verify
;(car (cons x y))
;(car (lamdba (m) (m x y)))
;((lambda (m) (m x y)) (lambda (p q) q))
;((lambda (p q) q) x y)
;x

(define (cons x y) 
  (lambda (m) (m x y)))

(define (car z) 
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

;test
;(define x (cons 1 2))
;(car x)
;;1
;(cdr x)
;;2
