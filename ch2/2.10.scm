(load "ch2/2.07.scm")

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) 
               (lower-bound y)))
        (p2 (* (lower-bound x) 
               (upper-bound y)))
        (p3 (* (upper-bound x) 
               (lower-bound y)))
        (p4 (* (upper-bound x) 
               (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (> (* (upper-bound y) (lower-bound y)) 0)
    (mul-interval x 
                  (make-interval 
                    (/ 1.0 (upper-bound y)) 
                    (/ 1.0 (lower-bound y))))
    (error "divide by an interval that spans zero
           interval " y)))

;test
;(define x (make-interval 4 8))
;(define y (make-interval 2 4))
;(div-interval x y)
