(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (next n)
  (if (= n 2)
    3
    (+ n 3)))
(define (divides? a b)
  (= (remainder b a) 0))

(define (square a) (* a a))

(define (prime? n)
  (= n (smallest-divisor n)))


(define (filtered-accumulate combiner null-value term a next b filter?)
  (define (select x)
    (if (filter? x)
      x
      null-value))
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combiner result (select (term a))))))
  (iter a null-value))

(define (sum term a next b)
  (accumulate + 0 term a next b)) 

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (libaobao a b)
  (define (term a) a)
  (define (next a) (+ a 1))
  (filtered-accumulate + 0 term a next b prime?))

(print (libaobao 2 8))
