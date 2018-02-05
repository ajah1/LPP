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

;----------------------------Actividad 3
(define (binario-a-decimal l)
  (cond
    ((null? (cdr l)) (* (car l) (expt 2 0)))
    (else
     (+ (* (car l) (expt 2 (- (length l) 1)))
        (binario-a-decimal (cdr l))))))