(define nil (list ))
(define (fringe items)
  (if (pair? items)
    (append (fringe (car items)) (fringe (cdr items)))
    (if (null? items) nil (list items))))

;test
;
;(define x 
;  (list (list 1 2) (list 3 4)))
;
;(fringe x)
;;(list 1 2 3 4)
;
;(fringe (list x x))
;;(list 1 2 3 4 1 2 3 4)
