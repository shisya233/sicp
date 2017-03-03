
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (car (cdr mobile)))
(define (branch-length branch) (car branch))
(define (branch-structure branch) (car (cdr branch)))
(define (weight branch)
  (if (pair? (branch-structure branch))
    (total-weight (branch-structure branch))
    (branch-structure branch)))
(define (total-weight mobile)
  (+ (weight (left-branch mobile))
     (weight (right-branch mobile))))

(define (balanced mobile)
  (define (balanced-sub sub) (if (pair? sub) (balanced sub) #t))
  (and (= (* (branch-length (left-branch mobile))
             (weight (left-branch mobile))
          (* (branch-length (right-branch mobile))
             (weight (right-branch mobile)))))
       (and (balanced-sub (branch-structure (left-branch mobile))) 
            (balanced-sub (branch-structure (left-branch mobile))))))

;test
;(define aa (make-branch 3 4))
;(define ab (make-branch 2 6))
;(define a (make-branch 10 (make-mobile aa ab)))
;(define b (make-branch 2 100))
;(define x (make-mobile a b))
;(define y (make-mobile aa a))
;(total-weight x)
;;110
;(balanced x)
;;#t
;(balanced y)
;;#f
