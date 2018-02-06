#lang r6rs
(import (rnrs base)
        (rnrs io simple)
        (schemeunit))

;----------------------------Actividad 1
(define (cuenta-simbolos l)
  (cond
    ((null? l) 0)
    ((symbol? (car l)) (+ 1 (cuenta-simbolos (cdr l))))
    (else (cuenta-simbolos (cdr l)))))

;; Demostracion
(display "\nActividad 1")
(display "\ncuenta-simbolos '(2 40 #f) => ")
(display (cuenta-simbolos '(2 40 #f) ))
;; Pruebas
(check-eq?  (cuenta-simbolos '(2 4 #f a)) 1)
(check-eq?  (cuenta-simbolos '(2 a "hola" #f hola)) 2)

;----------------------------Actividad 2
(define (menor x y)
  (if (< x y) x y))

(define (mayor x y)
  (if (> x y) x y))

(define (minimo lista)
  (cond
    ((null? (cdr lista)) (car lista))
    (else (menor (car lista)
                 (minimo (cdr lista))))))

;; Demostracion
(display "\n\nActividad 2")
(display "\nminimo '(9 8 6 4 3) => ")
(display (minimo '(9 8 6 4 3)))
;; Pruebas
(check-eq?  (minimo '(-9 0 3 -6 4)) -9)

; -----------------------ORDEN APLICATIVO
;(mayor (+ 2 3) (menor (- 10 2) (* 2 2)))
;(mayor 5 (menor 8 4))
;(mayor 5 (if (< 8 4) 8 4))
;(mayor 5 4)
;(if (> 5 4) 5 4)
;5

; ---------------------------ORDEN NORMAL
;(mayor (+ 2 3) (menor (- 10 2) (* 2 2)))
;
;(if (> (+ 2 3) (menor (- 10 2) (* 2 2)))
;    (+ 2 3)
;    (menor (- 10 2) (* 2 2)))
;
;(if (> (+ 2 3) (menor (- 10 2) (* 2 2)))
;    (+ 2 3)
;    (menor (- 10 2) (* 2 2)))
;
;(if (> (+ 2 3) (if (< (- 10 2) (* 2 2)) (- 10 2) (* 2 2)))
;    (+ 2 3)
;    (if (< (- 10 2) (* 2 2)) (- 10 2) (* 2 2)))
;
;(if (> 5 (if (< 8 4) 8 4)) 5 (if (< 8 4) 8 4))
;
;(if (> 5 4) 5 4)
;
;5

;----------------------------Actividad 3
(define (binario-a-decimal l)
  (cond
    ((null? (cdr l)) (* (car l) (expt 2 0)))
    (else
     (+ (* (car l) (expt 2 (- (length l) 1)))
        (binario-a-decimal (cdr l))))))

;; Demostracion
(display "\n\nActividad 3")
(display "\nbinario-a-decimal '(1 1 1 1) => ")
(display (binario-a-decimal '(1 1 1 1)))
;; Pruebas
(check-eq?  (binario-a-decimal '(1 1 0)) 6)
(check-eq?  (binario-a-decimal '(1 0)) 2)

;----------------------------Actividad 4
(define (member? elem l)
  (cond
    ((null? l) #f)
    ((eq? (car l) elem) #t)
    (else (member? elem (cdr l)))))

(define (repetidos? l)
  (cond
    ((null? l) #f)
    ((member? (car l) (cdr l)) #t)
    (else (repetidos? (cdr l)))))

;----------------------------Actividad 5

