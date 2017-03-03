(load "ch2/accumulate.scm")
(define (accumulate-n op init seqs)
  (define (car-n seqs)
    (if (null? seqs)
      nil
      (cons (car (car seqs))
            (car-n (cdr seqs)))))
  (define (cdr-n seqs)
    (if (null? seqs)
      nil
      (cons (cdr (car seqs))
            (cdr-n (cdr seqs)))))
  (if (null? (car seqs))
    nil
    (cons (accumulate op init (car-n seqs))
          (accumulate-n op init (cdr-n seqs)))))

;test
;(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
;(accumulate-n + 0 s)
;;(list 22 26 30)
