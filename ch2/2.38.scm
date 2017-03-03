(load "ch2/accumulate.scm")
(define fold-right accumulate)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest))
            (cdr rest))))
  (iter initial sequence))

;test
;(fold-right + 0 (list 1 2 3))
;;'print
;(fold-left  + 0 (list 1 2 3))
;;'print
;(fold-right list nil (list 1 2 3))
;;'print
;(fold-left  list nil (list 1 2 3))
;;'print
;
