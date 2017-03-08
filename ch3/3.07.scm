
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

(define (make-joint acc p1 p2)
  (define (dispatch passwd m)
    (if (eq? p2 passwd)
      (acc p1 m)
      (lambda (amount) "wrong password")))
  dispatch)

;test
;(define peter-acc (make-account 100 'open-sesame))
;((peter-acc 'open-sesame 'withdraw) 40)
;;60
;((peter-acc 'open-sesame 'deposit) 50)
;;110
;(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))
;((paul-acc 'rosebud 'withdraw) 40)
;;70
;((peter-acc 'open-sesame 'deposit) 40)
;;110
;((paul-acc 'otherpwd 'withdraw) 50)
;;'print
