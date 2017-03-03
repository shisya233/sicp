(load "ch2/2.07.scm")

(define (sub-interval x y)
  (make-interval (- (lower-bound x) 
                    (upper-bound y))
                 (- (upper-bound x) 
                    (lower-bound y))))

;test
;(define x (make-interval 4 8))
;(define y (make-interval 5 9))
;(define z (sub-interval y x))
;(lower-bound z)
;;-3
;(upper-bound z)
;;5
