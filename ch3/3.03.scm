
(define (make-account balance passwd)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance 
               (- balance amount))
             balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (error-pwd amount) "Incorrect password")
  (define (dispatch password m)
    (if (eq? passwd password)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            (else (error "Unknown request: 
                         MAKE-ACCOUNT" m)))
      error-pwd))
    dispatch)

;test
;(define acc (make-account 100 'secret-password))
;((acc 'secret-password 'withdraw) 40)
;;60
;((acc 'some-other-password 'deposit) 50)
;;'print

