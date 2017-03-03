(define (square x) (* x x))
(define nil (list ))

(define (square-list-a items)
  (if (null? items)
    nil
    (cons (square (car items))
          (square-list-a (cdr items)))))

(define (square-list-b items)
  (map (lambda (x) (square x)) items))


;test
;(square-list-a (list 1 2 3 4))
;;(list 1 4 9 16)
;(square-list-b (list 1 2 3 4))
;;(list 1 4 9 16)
