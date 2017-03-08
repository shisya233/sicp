
(define (apply-generic op . args)
  (let ((type-tags (car (map type-tag args))))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (error
          "No method for these types: 
          APPLY-GENERIC"
          (list op type-tags))))))

(define (attach-tag type-tag contents)
  (if (number? contents)
    contents
    (cons type-tag contents)))

(define (type-tag datum)
  (if (pair? datum)
    (car datum)
    (if (number? datum)
      `scheme-number
      (error "Bad tagged datum: 
             TYPE-TAG" datum))))

(define (contents datum)
  (if (pair? datum)
    (cdr datum)
    (if (number? datum)
      datum
      (error "Bad tagged datum: 
             CONTENTS" datum))))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put! 'add 'scheme-number
       (lambda (x y) (tag (+ x y))))
  (put! 'sub 'scheme-number
       (lambda (x y) (tag (- x y))))
  (put! 'mul 'scheme-number
       (lambda (x y) (tag (* x y))))
  (put! 'div 'scheme-number
       (lambda (x y) (tag (/ x y))))
  (put! 'equ? 'scheme-number
        (lambda (x y) (equal? x y)))
  (put! 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (equ? x y)
    (equal? (* (numer x) (denom y))
            (* (denom x) (numer y))))
  (define (tag x) (attach-tag 'rational x))
  (put! 'add 'rational
       (lambda (x y) (tag (add-rat x y))))
  (put! 'sub 'rational
       (lambda (x y) (tag (sub-rat x y))))
  (put! 'mul 'rational
       (lambda (x y) (tag (mul-rat x y))))
  (put! 'div 'rational
       (lambda (x y) (tag (div-rat x y))))
  (put! 'equ? `rational
        (lambda (x y) (equ? x y)))
  (put! 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-complex-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 
          'rectangular) 
     x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) 
     r a))
  (define (add-complex z1 z2)
    (make-from-real-imag 
      (+ (real-part z1) (real-part z2))
      (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag 
      (- (real-part z1) (real-part z2))
      (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang 
      (* (magnitude z1) (magnitude z2))
      (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang 
      (/ (magnitude z1) (magnitude z2))
      (- (angle z1) (angle z2))))
  (define (equ? z1 z2)
    (and (equal? (real-part z1) (real-part z2))
         (equal? (imag-part z1) (imag-part z2))))
  (define (tag z) (attach-tag 'complex z))
  (put! 'add 'complex
       (lambda (z1 z2) 
         (tag (add-complex z1 z2))))
  (put! 'sub 'complex
       (lambda (z1 z2) 
         (tag (sub-complex z1 z2))))
  (put! 'mul 'complex
       (lambda (z1 z2) 
         (tag (mul-complex z1 z2))))
  (put! 'div 'complex
       (lambda (z1 z2) 
         (tag (div-complex z1 z2))))
  (put! 'equ? 'complex
        (lambda (z1 z2) (equ? z1 z2)))
  (put! 'make-from-real-imag 'complex
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put! 'make-from-mag-ang 'complex
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  'done)

;test
;(install-scheme-number-package)
;(install-rational-package)
;(install-complex-package)
;(equ? 3 4)
;;#f
;(equ? 4 4)
;;#t
;(equ? (cons 'rational (cons 3 4)) (cons 'rational (cons 6 7)))
;;#f
;(equ? (cons 'rational (cons 3 4)) (cons 'rational (cons 6 8)))
;;#t
