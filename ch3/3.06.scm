(define (rand-update x)
  (remainder (+ 228 (* x 233)) 1000))

(define (make-rand init)
  (define generate
    (lambda ()
      (let ((x (rand-update init)))
        (set! init x)
        x)))
  (define reset
    (lambda (value) (set! init value)))
  (define dispatch
    (lambda (m)
      (cond ((eq? m 'generate) (generate))
            ((eq? m 'reset) reset))))
  dispatch)

;test
;(define rand (make-rand 233))
;(rand 'generate)
;;'print
;(rand 'generate)
;;'print
;((rand 'reset) 233)
;(rand 'generate)
;;'print
;(rand 'generate)
;;'print
