#lang r6rs
(import (rnrs)
        (schemeunit))


;;;;;;;;;;;;ABSTRACCION ARBOLES;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (dato-arbol arbol) 
    (car arbol))

(define (hijos-arbol arbol) 
    (cdr arbol))

(define (hoja-arbol? arbol) 
   (null? (hijos-arbol arbol)))

(define (construye-arbol dato lista-arboles)  
   (cons dato lista-arboles))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;ABSTRACCION ARBOLES BINARIOS;;;;;;;;;;;;;;;;;;
(define (dato-arbolb arbol)
   (car arbol))
   
(define (hijo-izq-arbolb arbol)
   (cadr arbol))

(define (hijo-der-arbolb arbol)
   (caddr arbol))
   
(define (vacio-arbolb? x)
   (null? x))

(define arbolb-vacio '())

(define (construye-arbolb dato hijo-izq hijo-der)
    (list dato hijo-izq hijo-der))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ACTIVIDAD 2
;(define arbol2 '(a (b (c (d)) (e)) (f)))
;(to-string-arbol arbol2) ⇒ "abcdef"
;;(A)
;;(B) fos

;; ACTIVIDAD 3 {fos, mutua, auxiliares}
;(ordenado-arbol? arbol)
;(ordenado-arbol? '(10 (5) (7))) ⇒ #t
;(ordenado-arbol? '(50 (10 (4) (6) (8)) (25 (15)))) ⇒ #t
;(ordenado-arbol? '(10 (8) (7))) ⇒ #f
;(ordenado-arbol? '(6 (5) (7))) ⇒ #f
;(ordenado-arbol? '(50 (10 (4) (6) (11)) (25) (15))) ⇒ #f

;; ACTIVIDAD 4
;; (A)
;(define arbol '(1 (1 (1)) (2 (3) (2)) (1 (2) (3))))
;(veces 1 2 arbol) => {4 . 3}
;(veces 4 3 arbol) => {0 . 2}
;(veces 4 5 arbol) => {0 . 0}

;; (B)
;Suponemos que lista contiene al menos un elemento
;(es-camino? '(a b a) arbol) ⇒ #t
;(es-camino? '(a b) arbol) ⇒ #f
;(es-camino? '(a b a b) arbol) ⇒ #f

;; (C)
;(nodos-nivel 0 arbol) ⇒ '(1)
;(nodos-nivel 1 arbol) ⇒ '(2 6)
;(nodos-nivel 2 arbol) ⇒ '(3 5 7)
;(nodos-nivel 3 arbol) ⇒ '(4 2)

;; ACTIVIDAD 5
;; (A)
;(define arbolb '(40 (18 (3 () ()) (23 () (29 () ()))) (52 (47 () ()) ())))
;(pertenece? 29 arbolb) ⇒ #t
;(pertenece? 42 arbolb) ⇒ #f

;; (B)
; AUXILIAR: 
;(menor-arbolb arbolb)
; AUXILIAR:
;(mayor-arbolb arbolb)
;(ordenado-arbolb? arbolb)

;; (C)
;(camino-b-tree b-tree '(= < < = > =)) ⇒ '(9 3 4)
;(camino-b-tree b-tree '(> = < < =)) ⇒ '(15 10)



