(define nil (list ))

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op 
                    initial 
                    (cdr sequence)))))


(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
              nil sequence))

(define (append seq1 seq2)
  (accumulate cons nil nil))

(define (length sequence)
  (accumulate (lambda (x y) (+ x y)) 0 sequence))

;test
;(define (square x) (* x x))
;(map square (list 2 3 4))
;;(list 4 9 16)
