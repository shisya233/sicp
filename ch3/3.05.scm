(define (square x) (* x x))
(define (rand n) (/ (random (* n 1000)) 1000))
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (rand range))))


(define (estimate-pi trials)
  (* 4.0 (monte-carlo trials 
                       estimate-integral)))
      
(define estimate-integral
  (lambda () (estimate P 2 8 4 10)))
(define (estimate P x1 x2 y1 y2)
  (P (random-in-range x1 x2)
     (random-in-range y1 y2)))
(define (P x y)
  (<= (+ (square (- x 5.0))
         (square (- y 7.0)))
      9.0))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) 
                 (+ trials-passed 1)))
          (else
            (iter (- trials-remaining 1) 
                  trials-passed))))
  (iter trials 0))
;test
;(estimate-pi 100000)
;;'print
