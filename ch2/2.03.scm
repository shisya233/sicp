(load "ch2/basic.scm")
(load "ch2/2.02.scm")

(define (make-rect-point A B)
  (cons (abs (- (x-point A) (x-point B)))
        (abs (- (y-point A) (y-point B)))))
(define (len-segment segment)
  (let ((x1 (x-point (start-segment segment)))
        (y1 (y-point (start-segment segment)))
        (x2 (x-point (end-segment segment)))
        (y2 (y-point (end-segment segment))))
    (sqrt (+ (square (- x1 x2)) (square (- y1 y2))))))
(define (make-rect-segment a b)
  (cons (len-segment a)
        (len-segment b)))

(define (la-rect rect) (car rect))
(define (lb-rect rect) (cdr rect))
(define (p-rect rect) (* 2 (+ (la-rect rect) (lb-rect rect))))
(define (a-rect rect) (* (la-rect rect) (lb-rect rect)))

;test
;(define p1 (make-point 0 0))
;(define p2 (make-point 0 4))
;(define p3 (make-point 9 0))
;(define seg1 (make-segment p1 p2))
;(define seg2 (make-segment p1 p3))
;(define rect1 (make-rect-point p2 p3))
;(define rect2 (make-rect-segment seg1 seg2))
;(p-rect rect1)
;;26
;(p-rect rect2)
;;26
;(a-rect rect1)
;;36
;(a-rect rect2)
;;36
