(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;(add-1 zero)
;(lambda (f) (lambda (x) (f ((zero f) x))))
;(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))
;(lambda (f) (lambda (x) (f ((lambda (x) x) x))))
;(lambda (f) (lambda (x) (f x)))

;(add-1 one)
;(lambda (f) (lambda (x) (f ((one f) x))))
;(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) f) x))))
;(lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
;(lambda (f) (lambda (x) (f (f x))))

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define (add a b) 
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))
(define (inc x) (+ x 1))
(define (cal n) ((n inc) 0))

;test
;(cal zero)
;;0
;(cal one)
;;1
;(cal two)
;;2
;(cal (add one two))
;;3
