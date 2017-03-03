(define (square x) (* x x))
(define nil (list ))
(define (square-tree-a items)
  (if (null? items)
    nil
    (if (pair? items)
      (cons (square-tree-a (car items))
            (square-tree-a (cdr items)))
      (square items))))

(define (square-tree-b items)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
           (square-tree-b sub-tree)
           (square sub-tree)))
       items))
;test
;
;(square-tree-a (list 1 (list 2 (list 3 4) 5) (list 6 7)))
;;(list 1 (list 4 (list 9 16) 25) (list 36 49))
;(square-tree-b (list 1 (list 2 (list 3 4) 5) (list 6 7)))
;;(list 1 (list 4 (list 9 16) 25) (list 36 49))
