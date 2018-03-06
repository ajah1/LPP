#lang r6rs
(import (rnrs)
        (schemeunit))

;;
;;--------------------------------------------------------------------------------- Actividad 1
;; 

;;(A)
(define (cuadrado l)
  (cuadrado-ite l '()))

(define (cuadrado-ite l res)
  (cond
    ((null? l) res)
    (else (cuadrado-ite (cdr l)
                        (cons (* (car l) (car l)) res)))))

;;(B)
;(max-lista '(2 5 9 12 5 0 4)) ⇒ 12
(define (max-lista l)
  (max-lista-ite (cdr l) (car l)))

(define (max-lista-ite l res)
  (cond
    ((null? l) res)
    ((> (car l) res) (max-lista-ite (cdr l) (car l)))
    (else (max-lista-ite (cdr l) res))))



;;
;;--------------------------------------------------------------------------------- Actividad 2
;; 

;(expande-pareja (cons 'a 4)) ⇒ {a a a a}
(define (expande-pareja p)
  (expande-pareja-ite p '()))

(define (expande-pareja-ite p res)
  (cond
    ((= (cdr p) 0) res)
    (else (expande-pareja-ite (cons (car p) (- (cdr p) 1))
                              (cons (car p) res)))))

;(expande '((#t . 3) ("LPP" . 2) (b . 4))) ⇒ {#t #t #t "LPP" "LPP" b b b b}
(define (expande lp)
  (expande-ite (cdr lp) (expande-pareja (car lp))))

(define (expande-ite lp res)
  (cond
    ((null? lp) res)
    (else (expande-ite (cdr lp)
                       (append res (expande-pareja (car lp)))))))


;;
;;--------------------------------------------------------------------------------- Actividad 3
;; 

;(aplica-funciones (list (cons sqrt 16) (cons list 2) (cons even? 5) (cons not #f))) 
; ⇒ {4 {2} #f #t}

(define (aplica-funciones lp)
  (aplica-funciones-ite lp '()))

(define (aplica-funciones-ite lp res)
  (cond
    ((null? lp) res)
    (else (aplica-funciones-ite (cdr lp)
                                (cons ((caar lp) (cdar lp)) res)))))


;;
;;--------------------------------------------------------------------------------- Actividad 4
;; 

;;(A)
;binario-a-decimal "101") ; ⇒ 5
(define (binario-a-decimal num)
  (binario-a-decimal-ite num 0))

(define (binario-a-decimal-ite num res)
  (cond
    ((equal? num "") res)))

;(binario-a-decimal "101101") ; ⇒ 45

;;(B)
;(decimal-a-hexadecimal 200) ; ⇒ "C8"
;(decimal-a-hexadecimal 999) ; ⇒ "3E7"