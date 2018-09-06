#lang r6rs
(import (rnrs)
        (schemeunit))
;------------------------------------------------------------------------ Actividad 1
;una lista con aquellos símbolos que tienen longitud impar.
(define (long-impar? dato resultado)
  (cond
    ((null? dato) '())
    ((odd? (string-length(string-upcase(symbol->string dato)))) (cons dato resultado))
    (else resultado)))

(define (longitud-impar ls)
  (fold-right long-impar? '() ls))

(display "Probando Actividad 1")
(display "\n    longitud-impar")

(check-equal? (longitud-impar '(me gusta LPP porque aprendo nuevos paradigmas de programación))
              '(gusta LPP aprendo))

;la suma de las longitudes de todos los símbolos de la lista.
 (define (suma-simbolos dato resultado)
   (+ (string-length(string-upcase(symbol->string dato)))
      resultado))
 
 (define (suma-longitudes lista-simbolos)
   (fold-right suma-simbolos 0 lista-simbolos))

(check-equal? (suma-longitudes '(me gusta LPP porque aprendo nuevos paradigmas de programación))
              53)

(display "\n    suma-longitudes")

;una lista con los símbolos escritos en mayúscula.
(define (mayusculas ls)
  (map string->symbol
       (map string-upcase
            (map symbol->string ls))))

(check-equal? (mayusculas '(me gusta LPP porque aprendo nuevos paradigmas de programación))
              '(ME GUSTA LPP PORQUE APRENDO NUEVOS PARADIGMAS DE PROGRAMACIÓN))

(display "\n    mayusculas")

;------------------------------------------------------------------------ Actividad 2
;;(A)
(define (resultado p)
  (cond
    ((> (car p) (cdr p)) 1)
    ((< (car p) (cdr p)) 2)
    (else 'X)))

(define (resultados‐quiniela lp)
  (map resultado lp))

(display "\n\nProbando Actividad 2")
(display "\n    resultados-quiniela")
         
(check-equal? (resultados‐quiniela '((2 . 2))) '(X))
(check-equal? (resultados‐quiniela '((3 . 2) (4 . 3) (3 . 5))) '(1 1 2))

;;(B)
(define (cuenta-resultados r lp)
  (length (filter (lambda (e) (equal? r e))
                  (resultados‐quiniela lp))))


(display "\n    cuenta-resultados")

(check-equal? (cuenta-resultados 1 '((1 . 2) (4 . 4) (3 . 5) (6 . 2) (9 . 9))) 1)
(check-equal? (cuenta-resultados 2 '((1 . 2) (4 . 4) (3 . 5) (6 . 2) (9 . 9))) 2)

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

(display "\n    cuenta-resultados-lista")

(check-equal? (cuenta-resultados-lista '((2 . 2))) '(0 1 0))
(check-equal? (cuenta-resultados-lista '((3 . 2) (4 . 3) (3 . 5))) '(2 0 1))



;------------------------------------------------------------------------ Actividad 3
(define l3 '((2 . 7) (3 . 5) (10 . 4) (5 . 5)))

(display "\n\nProbando Actividad 3")

;;(A) {1 {2 {3 0}}}
(define a3
  (fold-right list 0 '(1 2 3)))

(display "\n    (A)")
(check-equal? a3 (list 1 (list 2 (list 3 0))))

;;(B) {{{"hola" "como"} "estas"} "adios"}
(define b3
  (fold-left list "hola" '("como" "estas")))

;;(C) {8 14 10}
(define c3
  (filter even?
          (map (lambda (x) (+ (car x)
                              (cdr x)))
               l3)))

(display "\n    (C)")
(check-equal? c3 (list 8 14 10))

;;(D) {{7 . 2} {5 . 3}}
(define d3
  (filter (lambda (x) (> (car x) (cdr x)))
          (map (lambda (x) (cons (cdr x) (car x)))
               l3)))

(display "\n    (D)")
(check-equal? d3 (list (cons 7 2) (cons 5 3)))

;;(E) {3 10 5}
(define e3
  (fold-right (lambda (d r) (if (= d 0) r (cons d r)))
              '()
              (map (lambda (x) (if (even? (+ (car x)
                                             (cdr x)))
                                   (car x)
                                   0))
                   l3)))

(display "\n    (E)")
(check-equal? e3 (list 3 10 5))


;------------------------------------------------------------------------ Actividad 4
;;(A)
(display "\n\nProbando Actividad 4")

(define (construye-multiplicador k)
  (lambda (x)
    (* k x)))

(define (construye-exponenciador e)
  (lambda (b) (expt e b)))

(define multiplica-por-10 (construye-multiplicador 10))
(define multiplica-por-5 (construye-multiplicador 5))
(define elevado-2-a (construye-exponenciador 2))
(define elevado-5-a (construye-exponenciador 5))

(display "\n    multiplica-por-10")
(display "\n    multiplica-por-5")
(display "\n    elevado-2-a")
(display "\n    elevado-5-a")
(check-equal? (multiplica-por-10 3) 30)
(check-equal? (multiplica-por-5 3) 15)
(check-equal? (elevado-2-a 3) 8)
(check-equal? (elevado-5-a 3) 125)

;;(B)


;------------------------------------------------------------------------ Actividad 5
;;(A)
(define (suma-n-izq n lp)
  (map (lambda (p)
         (cons (+ (car p) n) (cdr p)))
       lp))

(display "\n\nProbando Actividad 5")
(display "\n    suma-n-izq")
(check-equal? (suma-n-izq 10 '((1 . 3) (0 . 9) (5 . 8) (4 . 1)))
              '((11 . 3) (10 . 9) (15 . 8) (14 . 1)))

;;(B)
(define (aplica-2 func lp)
  (map (lambda (p) (func (car p) (cdr p)))
       lp))

(display "\n    aplica-2")
(check-equal? (aplica-2 + '((2 . 3) (1 . -1) (5 . 4))) '(5 0 9))
(check-equal? (aplica-2 (lambda (x y)
             (if (even? x)
                 y
                 (* y -1))) '((2 . 3) (1 . 3) (5 . 4) (8 . 10))) '(3 -3 -4 10))



;------------------------------------------------------------------------ Actividad 6
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

(display "\n\nProbando Actividad 6")
(display "\n    cadena-mayor")

(check-equal? (cadena-mayor '("vamos" "a" "obtener" "la" "cadena" "mayor"))
              (cons "obtener" 7))
(check-equal? (cadena-mayor '("prueba" "con" "maximo" "igual"))
              (cons "maximo" 6))
(check-equal? (cadena-mayor '("hola"))
              (cons "hola" 4))














