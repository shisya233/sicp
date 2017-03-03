(load "ch2/accumulate.scm")
(load "ch2/enumerate-interval.scm")
(load "ch1/1.22.scm")
(define nil (list ))
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate 
                       (cdr sequence))))
        (else  (filter predicate 
                       (cdr sequence)))))
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) 
        (cadr pair) 
        (+ (car pair) (cadr pair))))

(define (uniquer-pairs n)
  (flatmap
    (lambda (i)
      (map (lambda (j) 
             (list i j))
           (enumerate-interval 
             1 
             (- i 1))))
    (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter 
         prime-sum?
         (uniquer-pairs n))))
;test
;(prime-sum-pairs 20)
;;'print
