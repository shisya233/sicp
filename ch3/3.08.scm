
(define (make-f)
  (let ((n 1))
    (define (f x)
      (if (= n x)
        1
        (begin (set! n 2) 0)))
    f))

;test
;(define f (make-f))
;(+ (f 0) (f 1))
;;0
;(define f (make-f))
;(+ (f 1) (f 0))
;;1
