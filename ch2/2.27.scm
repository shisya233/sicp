(define nil (list ))
(define (reverse items)
  (define (reverse-iter items ans)
    (if (null? (cdr items))
      (cons (car items) ans)
      (reverse-iter (cdr items) (cons (car items) ans))))
  (if (null? items)
    nil
    (reverse-iter items nil)))

(define (deep-reverse items)
  (define (iter items ans)
    (if (null? (cdr items))
      (cons (if (pair? (car items)) 
              (iter (car items) nil) 
              (car items)) ans)
      (iter (cdr items) 
            (cons (if (pair? (car items)) 
                    (iter (car items) nil) 
                    (car items)) ans))))
  (if (null? items)
    nil
    (iter items nil)))

;test
;(reverse (list 1 4 9 16 25))
;;(list 25 16 9 4 1)
;
;(define x (list (list 1 2) (list 3 4)))
;
;(reverse x)
;;(list (list 3 4) (list 1 2))
;
;(deep-reverse x)
;;(list (list 4 3) (list 2 1))
