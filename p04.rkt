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
;;(A)
(define (resultado p)
  (cond
    ((> (car p) (cdr p)) 1)
    ((< (car p) (cdr p)) 2)
    (else 'X)))

(define (resultados‐quiniela lp)
  (map resultado lp))

;;(B)
(define (cuenta-resultados r lp)
  (length (filter (lambda (e) (equal? r e))
                  (resultados‐quiniela lp))))

;;(C)
; auxiliares para incrementar la lista que cuenta resultados
(define (inc-gana l)
  (list (+ 1 (car l)) (cadr l) (caddr l)))
(define (inc-pierde l)
  (list (car l) (cadr l) (+ 1 (caddr l))))
(define (inc-empate l)
  (list (car l) (+ 1 (cadr l)) (caddr l)))

(define (cuenta dato resultado)
  (cond
    ((equal? dato 1) (inc-gana resultado))
    ((equal? dato 2) (inc-pierde resultado))
    (else (inc-empate resultado))))

(define (cuenta-resultados-lista lr)
  (fold-right cuenta (list 0 0 0) (resultados‐quiniela lr)))
  
;--------------------- Actividad 3
(define l3 '((2 . 7) (3 . 5) (10 . 4) (5 . 5)))

;;(A)
(define a3
  (fold-right list 0 '(1 2 3)))
;;(B)
(define b3
  (fold-left list "hola" '("como" "estas")))
;;(C)
(define c3
  (filter even?
          (map (lambda (x) (+ (car x)
                              (cdr x)))
               l3)))
;;(D)
(define d3
  (filter (lambda (x) (> (car x) (cdr x)))
          (map (lambda (x) (cons (cdr x) (car x)))
               l3)))
;;(E)
(define e3
  (fold-right (lambda (d r) (if (= d 0) r (cons d r)))
              '()
              (map (lambda (x) (if (even? (+ (car x)
                                             (cdr x)))
                                   (car x)
                                   0))
                   l3)))

;-------------------- Actividad 4
;;(A)
(define (make-multiplicador k)
  (lambda (x)
    (* k x)))

(define (make-exponenciador e)
  (lambda (b) (expt b e)))

;;(B)

;-------------------- Actividad 5

;;(A)
(define (suma-n-izq n lp)
  (map (lambda (p)
         (cons (+ (car p) n) (cdr p)))
       lp))

;;(B)
(define (aplica-2 func lp)
  (map (lambda (p) (func (car p) (cdr p)))
       lp))

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















