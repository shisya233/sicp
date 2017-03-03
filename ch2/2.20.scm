(define nil (list ))
(define (same-parity x . items)
  (define (same-parity? x y)
    (= (remainder x 2) (remainder y 2)))
  (define (filter x items)
    (if (null? items)
      nil
      (if (same-parity? x (car items))
        (cons (car items) (filter x (cdr items)))
        (filter x (cdr items)))))
  (cons x (filter x items)))

;test
;(same-parity 1 2 3 4 5 6 7)
;;(list 1 3 5 7)
;
;(same-parity 2 3 4 5 6 7)
;;(list 2 4 6)
