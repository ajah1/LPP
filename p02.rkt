#lang r6rs
(import (rnrs base)
        (rnrs io simple)
        (schemeunit))

;----------------------------Actividad 1
(define (menor x y)
  (if (< x y) x y))

(define (mayor x y)
  (if (> x y) x y))

(define (maximo lista)
  (cond
    ((null? (cdr lista)) (car lista))
    (else (mayor (car lista)
                 (maximo (cdr lista))))))

;; Demostracion
(display "\n\nActividad 1")
(display "\nmaximo '(9 8 6 4 3) => ")
(display (maximo '(9 8 6 4 3)))
;; Pruebas
(check-eq?  (maximo '(1 -1 3 6 4)) 6)


;----------------------------Actividad 2
(define (pertenece? elem l)
  (cond
    ((null? l) #f)
    ((eq? (car l) elem) #t)
    (else (pertenece? elem (cdr l)))))

(define (repetidos? l)
  (cond
    ((null? l) #f)
    ((pertenece? (car l) (cdr l)) #t)
    (else (repetidos? (cdr l)))))

;; Demostracion
(display "\n\nActividad 2")
(display "\nrepetidos? '(1 2 3 5 4 5 6) => ")
(display (repetidos? '(1 2 3 5 4 5 6)))
;; Pruebas
(check-eq? (repetidos? '(adios hola que tal)) #f)
(check-eq? (repetidos? '(#t #f #t #t #t)) #t)


;----------------------------Actividad 4
(define (binario-a-decimal l)
  (cond
    ((null? (cdr l)) (* (car l) (expt 2 0)))
    (else
     (+ (* (car l) (expt 2 (- (length l) 1)))
        (binario-a-decimal (cdr l))))))

;; Demostracion
(display "\n\nActividad 4")
(display "\nbinario-a-decimal '(1 1 1 1) => ")
(display (binario-a-decimal '(1 1 1 1)))
;; Pruebas
(check-eq?  (binario-a-decimal '(1 1 0)) 6)
(check-eq?  (binario-a-decimal '(1 0)) 2)


;----------------------------Actividad 5
(define (ordenada-creciente? l)
  (cond
    ((null? (cdr l)) #t)
    ((> (car l) (cadr l)) #f)
    (else (ordenada-creciente? (cdr l)))))

;; Demostracion
(display "\n\nActividad 5")
(display "\nordenada-creciente? '(-1 23 45 59 99) => ")
(display (ordenada-creciente? '(-1 23 45 59 99)))

;; Pruebas
(check-eq?  (ordenada-creciente? '(12 50 -1 293 1000)) #f)
(check-eq?  (ordenada-creciente? '(3)) #t)


;----------------------------Actividad 6
;(A)
(define (inc-izq p)
  (cons (+ 1 (car p))
        (cdr p)))

(define (inc-der p)
  (cons (car p)
        (+ 1 (cdr p))))

;(B)
(define (cuenta-impares-pares l)
  (cond
    ((null? l) (cons 0 0))
    ((odd? (car l)) (inc-izq (cuenta-impares-pares (cdr l))))
    ((even?(car l)) (inc-der (cuenta-impares-pares (cdr l))))))


;----------------------------Actividad 7
(define (cadena-mayor l)
  (cond
    ((null? l) (cons "" 0))
    ((> (string-length (car l)) (cdr (cadena-mayor (cdr l))))
     (cons (car l) (string-length (car l))))
    ((<= (string-length (car l)) (cdr (cadena-mayor (cdr l))))
     (cadena-mayor (cdr l)))))
  














