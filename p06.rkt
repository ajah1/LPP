#lang r6rs
(import (rnrs)
        (schemeunit))


;--------------------------------------------------------------------
;; barrera abstraccion
(define (hoja? dato)
   (not (list? dato)))
;--------------------------------------------------------------------

;; Ejercicio 1
;;(A)
(define lista-a '( (a b) d (c (e) (f g) h)))
(check-equal? (car (cdddar (cddr lista-a))) 'h)

;;(B)
(define lista-b '((1) ((6) ((3)) (10)) ((2)) (8)) )
(check-equal? (caaddr (cadr lista-b)) 10)

;; Ejercicio 2
;;(A)
;(cuenta-pares '(1 (2 3) 4 (5 6))) ; ⇒ 3
;(cuenta-pares '(((1 2) 3 (4) 5) ((((6)))))) ; ⇒ 3

(define (cuenta-pares l)
  (cond
    ((null? l) 0)
    ((hoja? l) (if (even? l) 1 0))
    (else (+ (cuenta-pares (car l))
             (cuenta-pares (cdr l))))
    ))

;;(B)
; aplanar la lista -- comprobar si son even -- contar los even
(define (cuenta-pares-fos l)
  (fold-right + 0
              (map (lambda (x) (if (hoja? x)
                                   (if (even? x) 1 0) (cuenta-pares-fos x)))
                   l)))

;;Ejercicio 3
;;(A)
;(cumplen-predicado even? '(1 (2 (3 (4))) (5 6))) ; ⇒ {2 4 6}
;(cumplen-predicado pair? '(((1 . 2) 3 (4 . 3) 5) 6)) ; ⇒ {{1 . 2} {4 . 3}
(define (cumplen-predicado pred l)
  (cond
    ((null? l) '())
    ((and (hoja? l) (pred l)) (list l))
    ((and (hoja? l) (not (pred l))) '())
    (else (append (cumplen-predicado pred (car l))
                (cumplen-predicado pred (cdr l)))
          )))

;;(B)
(define (cumplen-predicado-fos pred l)
  (fold-right append
              '()
              (map (lambda(x)
                     (if (hoja? x)
                         (if (pred x) (list x) '())
                         (cumplen-predicado-fos pred x))) l)))

;;Ejercicio 4
;;(A)
;(sustituye-elem  '(a b (c d (e c)) c (f (c) g))  'c  'h)
;⇒ {a b {h d {e h}} h {f {h} g}}
(define (sustituye-elem l elem-old elem-new)
  (cond
    ((null? l) '())
    ((and (hoja? l) (equal? l elem-old)) elem-new)
    ((and (hoja? l) (not(equal? l elem-old))) l)
    (else (cons (sustituye-elem (car l) elem-old elem-new)
                  (sustituye-elem (cdr l) elem-old elem-new)))))

;;Ejercicio 5
;(diff-listas '(a (b ((c)) d e) f) '(1 (b ((2)) 3 4) f)) ;⇒ {{a . 1} {c . 2} {d . 3} {e . 4}}
;(diff-listas '((a b) c) '((a b) c)) ⇒ ()
(define (diff-listas l1 l2)
  (cond
    ((null? l1) '())
    ((hoja? l1) (if (and (not(number? l1)) (number? l2))
                    (list (cons l1 l2)) '()))
    (else (append (diff-listas (car l1) (car l2))
                  (diff-listas (cdr l1) (cdr l2))))))



;;Ejercicio6
;;(A)
;(cuenta-hojas-debajo-nivel '(10 2 (4 6 (9 3) (8 7) 12) 1) 2) ; ⇒ 4
;(cuenta-hojas-debajo-nivel '(10 2 (4 6 (9 3) (8 7) 12) 1) 3) ; ⇒ 0

(define (cuenta-hojas-debajo-nivel lista n)
  0)