(define nil (list ))
(define (map proc items)
  (if (null? items)
    nil
    (cons (proc (car items))
          (map proc (cdr items)))))

;test
;(map abs (list -10 2.5 -11.6 17))
;;(list 10 2.5 11.6 17)
;
;(map (lambda (x) (* x x)) (list 1 2 3 4))
;;(list 1 4 9 16)
