(load "ch2/2.10.scm")
(load "ch2/2.11.scm")
(load "ch2/2.12.scm")

(define (add-interval x y)
  (make-interval (+ (lower-bound x) 
                    (lower-bound y))
                 (+ (upper-bound x) 
                    (upper-bound y))))

(define (par1 r1 r2)
  (div-interval 
    (mul-interval r1 r2)
    (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval 
      one
      (add-interval 
        (div-interval one r1) 
        (div-interval one r2)))))

;test
;(define A (make-center-percent 100 0.5))
;(define B (make-center-percent 200 3))
;(define C (div-interval A A))
;(define D (div-interval A B))
;(center C)
;;'print'
;(percent C)
;;'print'
;(center D)
;;'print'
;(percent D)
;;'print'
;(par1 A B)
;;'print'
;(par2 A B)
;;'print'
