(define (append! x y)
  (set-cdr! (last-pair x) (cons y '()))
  x)
(define (last-pair x)
  (if (null? (cdr x))
    x
    (last-pair (cdr x))))
(define (count-pairs x)
  (define (pair->list x tmp)
    (define (add tmp y)
      (if (not (memq y tmp))
        (append! tmp y)))
    (if (pair? x)
      (begin
        (add tmp x)
        (pair->list (car x) tmp)
        (pair->list (cdr x) tmp)
        tmp))
      tmp)
  (- (length (pair->list x '(x-list))) 1))

;test
;(define a (cons 1 2))
;(define b (cons 3 4))
;(define x (cons a b))
;(count-pairs x)
;;3
;(set-cdr! a b)
;(count-pairs x)
;;3
;(set-cdr! a b)
;(set-car! a b)
;(define y (cons a a))
;(count-pairs y)
;;3


