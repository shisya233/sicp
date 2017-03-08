(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (next n)
  (if (= n 2)
    3
    (+ n 2)))
(define (divides? a b)
  (= (remainder b a) 0))
(define (square a) (* a a))
(define (prime? n)
  (= n (smallest-divisor n)))
(define (find-prime n)
  (if (prime? n)
    (print n)
    (find-prime (+ n 1))))
(define (exp10 n)
  (if (= n 0)
    1
    (* 10 (exp10 (- n 1))))
  )

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-milliseconds)))

(define (start-prime-test n start-time)
  (if (prime? n)
    (report-prime (- (current-milliseconds) start-time))
    0))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  1)

(define (search-for-primes n count)
  (cond ((= count 0) (newline) (display "search completed") (newline))
        ((even? n) (search-for-primes (+ n 1) count))
        ((odd? n) (search-for-primes (+ n 2) (- count (timed-prime-test n))))))

;test
(search-for-primes 10000 5)
(search-for-primes 100000 5)
(search-for-primes 1000000 5)
(search-for-primes 10000000 5)
(search-for-primes 100000000 5)
(search-for-primes 10000000000 5)
(search-for-primes 1000000000000 5)
