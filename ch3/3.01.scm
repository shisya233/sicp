(define (make-accumulator n)
  (lambda (x)
    (begin (set! n (+ n x)) n)))

;test
;(define A (make-accumulator 5))
;(define B (make-accumulator 15))
;(A 10)
;;15
;(B 10)
;;25
;(A 10)
;;25
;(B -10)
;;15
