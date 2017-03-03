(load "ch2/2.36.scm")

(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))
(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (m) (matrix-*-vector cols m)) m)))

;test
;(define v1 (list 1 2 3))
;(define v2 (list 4 5 6))
;(dot-product v1 v2)
;;32
;(define m (list v1 v2))
;(matrix-*-vector m v1)
;;(list 14 32)
;(transpose m)
;;(list (list 1 4) (list 2 5) (list 3 6))
;(matrix-*-matrix m (transpose m))
;;'print
;(matrix-*-matrix (transpose m) m)
;;'print
