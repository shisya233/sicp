(define (for-each proc items)
  (if (null? items)
    #t
    (cons (proc (car items))
          (for-each proc (cdr items)))))

;test
;(for-each (lambda (x) (newline) (display x)) (list 57 321 88))
;
