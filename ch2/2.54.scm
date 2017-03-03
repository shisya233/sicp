(define (equal? a b)
    (cond ((null? a) (if (null? b) #t #f))
          ((not (pair? a)) (if (not (pair? b)) (eq? a b) #f))
          (else (if (not (pair? b))
                  #f
                  (and (equal? (car a) (car b))
                       (equal? (cdr a) (cdr b)))))))

;test
;(equal? '(this is a list) '(this is a list))
;;#t
;(equal? '(this is a list) '(this (is a) list))
;;#f
