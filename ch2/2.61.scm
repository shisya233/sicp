(define (adjoin-set x set)
    (cond ((null? set) (list x))
          ((= x (car set)) set)
          ((< x (car set)) (cons x set))
          (else (cons
                  (car set)
                  (adjoin-set x (cdr set))))))

;test
;(define a '())
;(define b '(3 5 6 8))
;
;(adjoin-set 4 a)
;;(list 4)
;(adjoin-set 3 b)
;;(list 3 5 6 8)
;(adjoin-set 4 b)
;;(list 3 4 5 6 8)
;(adjoin-set 8 b)
;;(list 3 5 6 8)
;(adjoin-set 10 b)
;;(list 3 5 6 8 10)
