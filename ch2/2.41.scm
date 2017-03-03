(load "ch2/accumulate.scm")
(load "ch2/enumerate-interval.scm")
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
(define (ij-pairs n)
  (flatmap
    (lambda (i)
      (map (lambda (j) 
             (list i j))
           (enumerate-interval 1 n)))
    (enumerate-interval 1 n)))
(define (f pair n s)
  (let ((i (car pair))
        (j (cadr pair)))
    (and (< (+ i j) s)
         (not (or (= i j) (= j (- s i j)) (= i (- s i j))))
         (< (- s (+ i j)) n))))

(define (make-pair-sum pair s)
  (list (car pair) 
        (cadr pair) 
        (- s (car pair) (cadr pair))))
(define (ijk-sum-to-s n s)
  (map (lambda (pair) (make-pair-sum pair s))
       (filter (lambda (pair) (f pair n s))
               (ij-pairs n))))

;test
;(ijk-sum-to-s 9 10)
;;'print'
