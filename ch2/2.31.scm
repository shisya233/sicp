(define (square x) (* x x))
(define nil (list ))

(define (tree-map proc items)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
           (tree-map proc sub-tree)
           (proc sub-tree)))
       items))

(define (square-tree tree) 
  (tree-map square tree))

;test
;
;(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
;;(list 1 (list 4 (list 9 16) 25) (list 36 49))
