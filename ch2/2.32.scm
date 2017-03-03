(define nil (list ))
(define (subsets s)
  (if (null? s)
    (list nil)
    (let ((rest (subsets (cdr s))))
      (append rest (map (lambda (x) (append (list (car s)) x)) rest)))))

;test
;(define set (list 1 2 3))
;(subsets set)
;;'print
