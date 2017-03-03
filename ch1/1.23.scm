(define (expmod base exp a)
  (cond ((= exp 0) 1)
        ((even? exp)
        (remainder (square (expmod base (/ exp 2) m)) m))
        (else 
          (remainder (* base (expmod base (- exp 1) m))) m)))
  

(define (prime? n)
  (= n (smallest-divisor n)))

(define (find-prime n)
  (if (prime? n)
    (print n)
    (find-prime (+ n 1)))
  )
(define (exp10 n)
  (if (= n 0)
    1
    (* 10 (exp10 (- n 1))))
  )
(newline)
(display (time (find-prime (exp10 13))))
(newline)
(display (time (find-prime (exp10 14))))
(newline)
(display (time (find-prime (exp10 15))))
