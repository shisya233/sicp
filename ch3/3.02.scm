(define (make-monitored f)
  (let ((count 0))
    (lambda (n)
      (cond ((eq? n 'how-many-calls?) count)
            ((eq? n 'reset-count) (set! count 0))
            (else (begin 
                    (set! count (+ count 1))
                    (f n)))))))


;test
;(define s (make-monitored sqrt))
;(s 100)
;;10.0
;(s 'how-many-calls?)
;;1
;(s 10000)
;;100.0
;(s 'how-many-calls?)
;;2
;(s 'reset-count)
;(s 'how-many-calls?)
;;0
