(define (make-account balance passwd)
  (define error-count 0)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance 
               (- balance amount))
             balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (error-pwd amount)
    (set! error-count (+ error-count 1))
    (if (> error-count 7)
      'call-the-cops
      "Incorrect password"))
  (define (dispatch password m)
    (if (eq? passwd password)
      (begin 
        (set! error-count 0)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request: 
                           MAKE-ACCOUNT" m))))
      error-pwd))
  dispatch)

;test
;(define acc (make-account 100 'secret-password))
;((acc 'secret-password 'withdraw) 40)
;;60
;((acc 'some-other-password 'deposit) 50)
;;'print
;((acc 'some-other-password 'deposit) 50)
;;'print
;((acc 'some-other-password 'deposit) 50)
;;'print
;((acc 'some-other-password 'deposit) 50)
;;'print
;((acc 'some-other-password 'deposit) 50)
;;'print
;((acc 'some-other-password 'deposit) 50)
;;'print
;((acc 'some-other-password 'deposit) 50)
;;'print
;((acc 'some-other-password 'deposit) 50)
;;'print
;((acc 'secret-password 'withdraw) 40)
;;20
;((acc 'some-other-password 'deposit) 50)
;;'print

