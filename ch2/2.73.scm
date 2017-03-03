

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2) 
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? n num) (and (number? n) (eq? n num)))
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
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) 
         (if (same-variable? exp var) 
           1 
           0))
        (else ((get 'deriv (operator exp)) 
               (operands exp) 
               var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (install-add-package)

  (define (addend s) (car s)) 
  (define (augend s) (cdr s)) 
  (define (make-sum n1 n2)
    (cond ((=number? n1 0) n2)
          ((=number? n2 0) n1)
          ((and (number? n1) (number? n2)) (+ n1 n2))
          (else (tag (cons n1 n2)))))
  (define (deriv-plus exp var)
    (make-sum (deriv (addend exp) var)
                    (deriv (augend exp) var)))
  (define (tag x) (attach-tag '+ x))
  (put! 'deriv '+ deriv-plus)
  (put! 'make '+ 
        (lambda (n1 n2)
          (make-sum n1 n2)))
  'done)
(define (install-product-package)

  (define (multiplier s) (car s)) 
  (define (multiplicand s) (cdr s)) 
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (tag (cons m1 m2)))))
  (define (deriv-product exp var)
    ((get 'make '+)
     (make-product (deriv (multiplier exp) var) (multiplicand exp))
     (make-product (deriv (multiplicand exp) var) (multiplier exp))))
  (define (tag x) (attach-tag '* x))
  (put! 'deriv '* deriv-product)
  (put! 'make '* 
        (lambda (n1 n2)
          (make-product n1 n2)))
  'done)

(define (install-exp-package)
  (define (base x) (car x))
  (define (exponent x) (cdr x))
  (define (make-exponentiation b e) 
    (cond ((=number? e 0) 1)
          ((=number? e 1) b)
          ((and (number? b) (number? e)) (expt b e))
          (else (tag (cons b e))))) 
  (define (deriv-exp ex var)
    ((get 'make '*) 
     ((get 'make '*) 
      (exponent ex)
      (make-exponentiation (base ex) (- (exponent ex) 1)))
     (deriv (base ex) var)))
  (define (tag x) (attach-tag '** x))
  (put! 'deriv '** deriv-exp)
  (put! 'make '**
        (lambda (b e)
          (make-exponentiation b e)))
  'done)
;test
;(install-add-package)
;;'done
;(install-product-package)
;;'done
;(install-exp-package)
;;'done
;(define exp1 ((get 'make '+) 'x 'y))
;;'print
;(deriv exp1 'x)
;;1
;(deriv ((get 'make '**) ((get 'make '+) 'x 'y) 2) 'x)
;;'print
