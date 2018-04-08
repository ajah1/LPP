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

;; ACTIVIDAD 1

;; A.1
(define arbol '(15 (4 (2) (3))
                   (8 (6))
                   (12 (9) (10) (11))))
(check-equal?  (caar(cddar(hijos-arbol(hijos-arbol(hijos-arbol arbol)))))
                10)

;; A.2
;1. la invocación a (hijos-datos-arbol (car bosque))
;   de dentro de la funcion devuelve la suma
;   de los datos del primer arbol del bosque (4 + 2 + 3 = 9)
;
;2. la primera llamada a recursiva a suma-datos-bosque,
;   suma el dato de todos los arboles del bosque menos el primero

;; A.3
;1. la función map devuelve una lista aplanada de listas (cada nodo)
;
;
; 2. 
;   

;; B.1
(define arbolb (construye-arbolb 40
                                  (construye-arbolb 23
                                                     (construye-arbolb 5 '() '())
                                                     (construye-arbolb 32
                                                                       (construye-arbolb 29 '() '())
                                                                       '()))
                                  (construye-arbolb 45 '()
                                                    (construye-arbolb 56 '() '()))))

(check-equal? (dato-arbolb(hijo-izq-arbolb (hijo-der-arbolb (hijo-izq-arbolb arbolb))))
              29)


;; ACTIVIDAD 2
;;(A)
(define arbol2 '(a (b (c (d)) (e)) (f)))

(define (to-string-arbol arbol)
  (string-append (symbol->string (dato-arbol arbol))
                 (to-string-bosque (hijos-arbol arbol))))

(define (to-string-bosque bosque)
  (cond
    ((null? bosque) "")
    (else (string-append (to-string-arbol (car bosque))
                         (to-string-bosque (cdr bosque))))))

;;(B) fos
(define (to-string-fos arbol)
  (string-append (symbol->string(dato-arbol arbol))
        (fold-right string-append ""
                    (map to-string-fos (hijos-arbol arbol)))))

;; ACTIVIDAD 3 {fos, mutua, auxiliares}
;(ordenado-arbol? arbol)
;(ordenado-arbol? '(10 (5) (7))) ⇒ #t
;(ordenado-arbol? '(50 (10 (4) (6) (8)) (25 (15)))) ⇒ #t
;(ordenado-arbol? '(10 (8) (7))) ⇒ #f
;(ordenado-arbol? '(6 (5) (7))) ⇒ #f
;(ordenado-arbol? '(50 (10 (4) (6) (11)) (25) (15))) ⇒ #f

; AUXILIAR: comprueba cosas
(define (comprueba raiz bosque)
  (cond
    ((null? bosque) #t)
    ((> raiz (caar bosque)) (comprueba raiz (cdr bosque)))
    (else #f)))

; AUXILIAR: compruebas mas cosas
(define (comprueba-mas bosque)
  (cond
    ((null? (cdr bosque)) #t)
    ((< (caar bosque) (cadr bosque))
     (comprueba-mas (cdr bosque)))
    (else #f)))
     

(define (ordenado-arbol? arbol)
  (and (comprueba (dato-arbol arbol) (hijos-arbol arbol))
       (ordenado-bosque? (hijos-arbol arbol))))

(define (ordenado-bosque? bosque)
  #t)

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



