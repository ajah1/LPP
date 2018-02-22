#lang r6rs
(import (rnrs)
        (schemeunit))


;--------------------- Actividad 1
(define (aplica-funciones lp)
  (cond
    ((null? lp) '())
    (else (cons ((caar lp) (cdar lp))
                (aplica-funciones (cdr lp))))))

(define (operar pareja)
  ((car pareja) (cdr pareja)))

(define (aplica-funciones-fos lp)
  (map operar lp))


;--------------------- Actividad 4
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

