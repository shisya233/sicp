(define (cont-frac n d k)
  (define (f-iter i ans)
    (if (= i 1)
      (/ (n i) (+ (d i) ans))
      (f-iter (- i 1)
              (/ (n i) (+ (d i) ans)))))
  (f-iter k 0))

(define (fi k)
  (/ 1.0
     (cont-frac (lambda (i) 1.0)
                (lambda (i) 1.0)
                k)))
