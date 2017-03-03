;test
(list 'a 'b 'c)
;(list 'a 'b 'c)
(list (list 'george))
;(list (list 'george))
(cdr '((x1 x2) (y1 y2)))
;(list '(y1 y2))
(cadr '((x1 x2) (y1 y2)))
;(list 'y1 'y2)
(pair? (car '(a short list)))
;#f
(memq 'red '((red shoes) (blue socks)))
;#f
(memq 'red '(red shoes blue socks))
;'(red shoes blue socks)
