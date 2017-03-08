(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op (car type-tags))))
      (if proc
        (apply proc (map contents args))
        (if (= (length args) 2)
          (let ((type1 (car type-tags))
                (type2 (cadr type-tags))
                (a1 (car args))
                (a2 (cadr args)))
            (if (equal? type1 type2)
              (error 
                "No method for 
                these types"
                (list 
                  op 
                  type-tags))
              (let ((t1->t2 
                      (get-coercion type1
                                    type2))
                    (t2->t1 
                      (get-coercion type2 
                                    type1)))
                (cond (t1->t2
                        (apply-generic 
                          op (t1->t2 a1) a2))
                      (t2->t1
                        (apply-generic 
                          op a1 (t2->t1 a2)))
                      (else
                        (error 
                          "No method for 
                          these types"
                          (list 
                            op 
                            type-tags)))))))
              (error 
                "No method for these types"
                (list op type-tags)))))))
(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
    (car datum)
    (error "Bad tagged datum: 
           TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
    (cdr datum)
    (error "Bad tagged datum: 
           CONTENTS" datum)))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (raise x) (apply-generic 'raise x))

(define (install-integer-package)
  (define (make-integer n) (round n))
  (define (tag x) (attach-tag 'integer x))
  (put! 'add 'integer
        (lambda (x y) (tag (make-integer (+ x y)))))
  (put! 'sub 'integer
        (lambda (x y) (tag (make-integer (- x y)))))
  (put! 'mul 'integer
        (lambda (x y) (tag (make-integer (* x y)))))
  (put! 'div 'integer
        (lambda (x y) (tag (make-integer (/ x y)))))
  (put! 'make 'integer
        (lambda (x) (tag (make-integer x))))
  (put! 'raise 'integer
        (lambda (x) ((get 'make 'rational ) x 1)))
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
  (define (raise x)
    ((get 'make 'real) (/ (numer x) (denom x))))
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
  (put! 'raise 'rational
        (lambda (x) (raise x)))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))
(define (install-real-package)
  (define (make-real n) n)
  (define (tag x) (attach-tag 'real x))
  (define (raise x)
    ((get make-from-real-imag 'complex) x 0))
  (put! 'make 'real
        (lambda (x) (tag (make-real x))))
  (put! 'raise 'real
        (lambda (x) (raise x)))
  'done)

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
;(install-integer-package)
;(install-rational-package)
;(install-real-package)
;(define a ((get 'make 'integer) 3.3))
;(define b ((get 'make 'integer) 8.3))
;(add a b)
;;(cons 'integer 11.0)
;(raise a)
;;(cons 'rational (cons 3.0 1))
;(raise (raise a))
;;(cons 'real 3.0)
