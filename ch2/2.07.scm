(define (make-interval a b) (cons a b))
(define (lower-bound x)
  (let ((a (car x))
        (b (cdr x)))
    (if (< a b) a b)))
(define (upper-bound x)
  (let ((a (car x))
        (b (cdr x)))
    (if (> a b) a b)))

;test
;(define x (make-interval 4 8))
;(lower-bound x)
;;4
;(upper-bound x)
;;8
