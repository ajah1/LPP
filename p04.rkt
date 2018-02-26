#lang r6rs
(import (rnrs)
        (schemeunit))

;--------------------- Actividad 1

;una lista con aquellos símbolos que tienen longitud impar.
(define (long-impar? dato resultado)
  (cond
    ((null? dato) '())
    ((odd? (string-length(string-upcase(symbol->string dato)))) (cons dato resultado))
    (else resultado)))

(define (longitud-impar ls)
  (fold-right long-impar? '() ls))

;la suma de las longitudes de todos los símbolos de la lista.
 (define (suma-simbolos dato resultado)
   (+ (string-length(string-upcase(symbol->string dato)))
      resultado))
 
 (define (suma-longitudes lista-simbolos)
   (fold-right suma-simbolos 0 lista-simbolos))

;una lista con los símbolos escritos en mayúscula.
(define (mayusculas ls)
  (map string->symbol
       (map string-upcase
            (map symbol->string ls))))


;--------------------- Actividad 2

;(resultados‐quiniela lista‐parejas)
;(cuenta-resultados resultado lista-resultados)
;(cuenta-resultados-lista lista-resultados) de la práctica 3.

;-------------------- Actividad 4

;;(A)
(define (make-multiplicador k)
  (lambda (x)
    (* k x)))

(define (make-exponenciador e)
  (lambda (b) (expt b e)))

;-------------------- Actividad 5

;;(A)
(define (suma-n-izq n lp)
  (map (lambda (p)
         (cons (+ (car p) n) (cdr p)))
       lp))

;;(B)

;-------------------- Actividad 6

; aux para usar con map
(define (juntar cadena)
  (cons cadena (string-length cadena)))

; funcion de plegado para el fold-right
(define (compara-cadena dato resultado)
  (if (> (cdr dato) (cdr resultado))
      dato
      resultado))

(define (cadena-mayor lc)
  (fold-right compara-cadena (cons "" 0) (map juntar lc)))















