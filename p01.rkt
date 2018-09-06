#lang r6rs
(import (rnrs base)
        (rnrs io simple)
        (schemeunit))

;---------------------------------------------------Actividad 1

;(A)
(define (b-a-d b3 b2 b1 b0)
  (+ (* b3 (expt 2 3))
     (* b2 (expt 2 2))
     (* b1 (expt 2 1))
     (* b0 (expt 2 0))))

;(B)
(define (b-a-h b3 b2 b1 b0)
  (cond
    ( (<= (b-a-d b3 b2 b1 b0) 9) (b-a-d b3 b2 b1 b0) )
    ( (= (b-a-d b3 b2 b1 b0) 10) "A" )
    ( (= (b-a-d b3 b2 b1 b0) 11) "B" )
    ( (= (b-a-d b3 b2 b1 b0) 12) "C" )
    ( (= (b-a-d b3 b2 b1 b0) 13) "D" )
    ( (= (b-a-d b3 b2 b1 b0) 14) "E" )
    ( (= (b-a-d b3 b2 b1 b0) 15) "F" )))
    
;; Demostracion
(display "\nActividad 1")
(display "\nBinario a decimal     1 1 0 1 => ")
(display (b-a-d 1 1 0 1))
(display "\nBinario a hexadecimal 1 1 0 1 => ")
(display (b-a-h 1 1 0 1))


;; Pruebas
(check-eq?  (b-a-d 1 1 1 1)  15)
(check-eq?  (b-a-d 0 1 1 0)  6)
(check-eq?  (b-a-d 0 0 1 0)  2)

(check-eq?  (b-a-h 1 1 1 1)  "F")
(check-eq?  (b-a-h 0 1 1 0)  6)
(check-eq?  (b-a-h 1 0 1 0)  "A")


;----------------------------------------------- Actividad 4

; suma los valores que componen una pareja
(define (suma-tirada tirada)
  (+ (car tirada)
     (cdr tirada)))

(define (tirada-ganadora t1 t2)
  (cond
   ( (< (abs (- (suma-tirada t1) 7)) (abs (- (suma-tirada t2) 7)) ) 1)
   ( (> (abs (- (suma-tirada t1) 7)) (abs (- (suma-tirada t2) 7)) ) 2)
   ( else 0)))
;; Demostracion
(display "\n\nActividad 4")
(display "\ntirada-ganadora (1 3) (1 6) => ")
(display (tirada-ganadora (cons 1 3) (cons 1 6)))

;; Pruebas
(check-equal? (tirada-ganadora (cons 1 5) (cons 2 2)) 1)
(check-equal? (tirada-ganadora (cons 6 2) (cons 3 3)) 0)

;----------------------------------------------- Actividad 5

; devuelve la distancia entre dos puntos
(define (distancia x1 y1 x2 y2)
  (sqrt
   (+ (expt (- x2 x1) 2)
      (expt (- y2 y1) 2))))

; calculo del espacio recorrido
(define (espacio v)
  (*
   (* v (/ 1000 3600))
   (/ 50000 (expt v 2))))

(define (dentro-alcance? x1 x2 y1 y2 v)
  (if (< (distancia x1 x2 y1 y2) (espacio v))
          #t
          #f))

;;Demostracion
(display "\n\nActividad 5")
(display "\ndentro-alcance? 0 0 500 500 30 => ")
(display (dentro-alcance? 0 0 500 500 30))

;; Pruebas
(check-eq?  (dentro-alcance? 0 0 500 500 30)  #f)
(check-eq?  (dentro-alcance? 100 200 500 500 20) #t)



;----------------------------------------------- Actividad 6

; compara dos número naturales
(define epsilon 0.0001)
(define (i-r? x y)
  (< (abs (- x y)) epsilon))

; calcula la distancia entre dos parejas de puntos
(define (dp p1 p2)
  (sqrt
   (+ (expt (- (car p2) (car p1) ) 2)
      (expt (- (cdr p2) (cdr p1) ) 2))))

(define (tipo-triangulo p1 p2 p3)
  (cond
    ((equal? (and (i-r?(dp p1 p2) (dp p1 p3)) (i-r?(dp p3 p2) (dp p1 p2)))
             #t) "equilatero")
    ((equal? (or (i-r?(dp p1 p2) (dp p1 p3)) (i-r?(dp p3 p2) (dp p1 p2)))
             #f) "escaleno")
    (else "isosceles")))


;; Demostracion
(display "\n\nActividad 6")
(display "\n(tipo-triangulo (cons -2 3) (cons  2 6) (cons 5 3)) => ")
(display (tipo-triangulo (cons -2 3) (cons  2 6) (cons 5 3)))

;; Pruebas
(check-eq? (tipo-triangulo (cons 1 1) (cons 1 6) (cons 6 1)) "isosceles")
(check-eq? (tipo-triangulo (cons -2 3) (cons 2 6) (cons 5 3)) "escaleno")
(check-eq? (tipo-triangulo (cons -3 0) (cons 3 0) (cons 0 5.1961)) "equilatero")



;----------------------------------------------- Actividad 7

(define (calculadora l)
  (cond
    ( (equal? (car l) #\+) (+ (car(cdr l)) (car(cddr l)) ))
    ( (equal? (car l) #\*) (* (car(cdr l)) (car(cddr l)) ))
    ( (equal? (car l) #\-) (- (car(cdr l)) (car(cddr l)) ))
    ( (equal? (car l) #\/) (/ (car(cdr l)) (car(cddr l)) ))))

;;Demostracion
(display "\n\nActivdad 7")
(display "\ncalculadora (list * 3 3) => ")
(display (calculadora (list #\* 3 3)))

;;Pruebas
(check-eq? (calculadora (list #\+ 2 3)) 5)
(check-eq? (calculadora (list #\* 3 4)) 12)
(check-eq? (calculadora (list #\- 7 4)) 3)
(check-eq? (calculadora (list #\/ 6 3))  2)
