(load "ch2/2.07.scm")

(define (make-center-percent c p)
  (let ((tolerance (* c (/ p 100.0))))
    (make-interval (- c tolerance) (+ c tolerance))))

(define (center i)
  (/ (+ (lower-bound i) 
        (upper-bound i)) 
     2))

(define (percent i)
  (* (/ (/ (- (upper-bound i)
              (lower-bound i))
           2)
        (center i))
     100))

;test
;(define x (make-center-percent 100 2.5))
;(center x)
;;100.0
;(percent x)
;;2.5
