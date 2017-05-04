(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (set-front-ptr! item)
      (set! front-ptr item))
    (define (set-rear-ptr! item)
      (set! rear-ptr item))
    (define (empty-queue?) (null? front-ptr))
    (define (front-queue) (car front-ptr))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
                (set! front-ptr new-pair)
                (set! rear-ptr new-pair))
              (else
                (set-cdr! rear-ptr new-pair)
                (set! rear-ptr new-pair)))))
    (define (delete-queue!)
      (set! front-ptr (cdr front-ptr)))
    (define (dispatch m)
      (cond ((eq? m 'front-ptr) front-ptr)
            ((eq? m 'rear-ptr) rear-ptr)
            ((eq? m 'set-front-ptr!) (lambda (item) (set-front-ptr! item)))
            ((eq? m 'set-rear-ptr!) (lambda (item) (set-rear-ptr! item)))
            ((eq? m 'empty-queue?) (empty-queue?))
            ((eq? m 'front-queue) (front-queue))
            ((eq? m 'insert-queue!) (lambda (item) (insert-queue! item)))
            ((eq? m 'delete-queue!) (delete-queue!))))
            ;(else (error "unknown lable" m))
    dispatch))

(define (front-ptr queue) (queue 'front-ptr))
(define (rear-ptr queue) (queue 'rear-ptr))
(define (set-front-ptr! queue item) ((queue 'set-front-ptr!) item))
(define (set-rear-ptr! queue item) ((queue 'set-rear-ptr!) item))
(define (empty-queue? queue) (queue 'empty-queue?))
(define (front-queue queue)
  (if (empty-queue? queue)
    (error "FRONT called with
           empty queue" queue)
    (queue 'front-queue)))
(define (insert-queue! queue item) ((queue 'insert-queue!) item))
(define (delete-queue! queue)
  (if (empty-queue? queue)
    (error "DELETE! called with
           empty queue" queue)
    (queue 'delete-queue!)))

;test
;(define q (make-queue))
;(if (empty-queue? q) 1 2)
;;'print
;(front-ptr q)
;;'print
;(rear-ptr q)
;;'print
;(insert-queue! q 'aaaaa)
;(insert-queue! q 'aaa)
;(insert-queue! q 'aaaa)
;(front-ptr q)
;;'print
;(rear-ptr q)
;;'print
;(empty-queue? q)
;;'print
;(delete-queue! q)
;(front-ptr q)
;;'print
;(rear-ptr q)
;;'print
