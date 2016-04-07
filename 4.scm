;;;
;(add-load-path "C:\\Users\\nobu\\Desktop")
;;(load "4_2mceval.scm")
;;;; 4.1
;(define (list-of-values exps env)
;  (if (no-operands? exps)
;      '()
;      (cons (eval (first-operand exps) env)
;            (list-of-values (rest-operands exps) env))))
; 
;(define val 10)
;(define ex '((set! val (* val 5)) (set! val (+ val 2))))
;;;; ---->
;(define (to-right-list-of-values exps)
;  (if (null? exps)
;      '()
;      (cons (eval (car exps) interaction-environment)
;            (to-right-list-of-values (cdr exps)))))
;(to-right-list-of-values ex)
;;;; <---
;(define (to-left-list-of-values exps)
;  (if (null? exps)
;      '()
;      (let ((hoge (to-left-list-of-values (cdr exps))))
;        (cons (eval (car exps) interaction-environment)
;              hoge))))
;(to-left-list-of-values ex)
; 
; 
;;;;4.2
;;;;a:\•¶ƒGƒ‰[‚É‚È‚é‚Ì‚Å‚Í‚Æ
;;;;b
;(load "4_2mceval.scm")

;;;4.3
;; (add-load-path "C:\\Users\\nobu\\Desktop\\emacs")
;; (add-load-path "C:\\Users\\nobu\\Desktop")
;;(load "4_3mceval.scm")

;;;4.6
(add-load-path "C:\\Users\\nobu\\Desktop")
(load "4_6mceval.scm")
