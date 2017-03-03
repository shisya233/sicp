
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set) (cons x set))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) 
         '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) 
                                 set2)))
        (else (intersection-set (cdr set1) 
                                set2))))
(define (union-set set1 set2)
  (if (null? set1)
    set2
    (cons (car set1)
          (union-set (cdr set1) set2))))


;test
;(define a '(3 6 5 4))
;(define b '(3 4 9 8 7))
;(intersection-set a b)
;;'(3 4)
;(union-set a b)
;;'(3 6 5 4 3 4 9 8 7)
