
(load "ch2/2.38.scm")

(define (reverse-r sequence)
  (fold-right 
    (lambda (x y) (append y (list x))) nil sequence))

(define (reverse-l sequence)
  (fold-left 
    (lambda (x y) (cons y x)) nil sequence))

;test
;(define s (list 1 2 3))
;(reverse-r s)
;;(list 3 2 1)
;(reverse-l s)
;;(list 3 2 1)
