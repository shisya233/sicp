(define nil (list ))
(define (reverse items)
  (define (reverse-iter items ans)
    (if (null? (cdr items))
      (cons (car items) ans)
      (reverse-iter (cdr items) (cons (car items) ans))))
  (if (null? items)
    nil
    (reverse-iter items nil)))

;test
;(reverse (list 1 4 9 16 25))
;;(list 25 16 9 4 1)
