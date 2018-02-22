#lang r6rs
(import (rnrs base)
        (rnrs io simple)
        (schemeunit))

;-------------------- Actividad 4

(define (sum-izq n p)
  (cons (+ n (car p))
        (cdr p)))

(define (sum-der n p)
  (cons (car p)
        (+ n (cdr p))))


(define (suma-impares-pares l)
  (cond
    ((null? l) (cons 0 0))
    ((odd? (car l)) (sum-izq (car l) (suma-impares-pares (cdr l))))
    (else (sum-der (car l) (suma-impares-pares (cdr l))))))


;-------------------- Actividad 5

;; Devuelven #t o #f
(define (ganar partido)
  (if (> (car partido) (cdr partido)) #t #f))

(define (perder partido)
  (if (< (car partido) (cdr partido)) #t #f))

;; incremeta los resultados
(define (inc-gana l)
  (list (+ 1 (car l))
        (cadr l)
        (caddr l)))
(define (inc-pierde l)
  (list (car l)
        (cadr l)
        (+ 1 (caddr l))))
(define (inc-empate l)
  (list (car l)
        (+ 1 (cadr l))
        (caddr l)))

(define (resultados l)
  (cond
    ((null? l) '(0 0 0))
    ((ganar (car l)) (inc-gana (resultados (cdr l))))
    ((perder (car l)) (inc-pierde (resultados (cdr l))))
    (else (inc-empate (resultados (cdr l))))))


;-------------------- Actividad 6

(define (crea-persona nomb edad sexo)
  (cons nomb (cons edad sexo)))

(define (nomb p) (car p))
(define (edad p) (cadr p))
(define (sexo p) (cddr p))

(define (edad-total people)
  (cond
    ((null? people) 0)
    ((null? (cdr people)) (edad (car people)))
    (else (+ (edad-total (car people))
             (edad-total (cdr people))))))

(define (edad-media personas)
  (/ (edad-total personas) (string-length personas)))

(define personas (list (crea-persona "Juan" 23 'hombre)
                       (crea-persona "Ana" 18 'mujer)
                       (crea-persona "Pablo" 30 'hombre)
                       (crea-persona "Paula" 20 'mujer)
                       (crea-persona "Antonio" 25 'hombre)))








