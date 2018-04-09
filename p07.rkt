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

; AUXILIAR: raiz mayor que el dato de sus hijos
(define (comprueba raiz bosque)
  (cond
    ((null? bosque) #t)
    ((> raiz (dato-arbol(car bosque))) (comprueba raiz (cdr bosque)))
    (else #f)))

(check-equal? (comprueba (dato-arbol '(10 (5) (7))) (hijos-arbol '(10 (5) (7))))  #t)
(check-equal? (comprueba (dato-arbol '(6 (5) (7))) (hijos-arbol '(6 (5) (7)))) #f)

; AUXILIAR: los datos de los hijos están ordenados
(define (comprueba-mas bosque)
  (cond
    ((or (null? bosque) (null? (cdr bosque))) #t)
    ((< (dato-arbol(car bosque)) (caadr bosque))
     (comprueba-mas (cdr bosque)))
    (else #f)))

(check-equal? (comprueba-mas (hijos-arbol '(50 (10 (4) (6) (11)) (25) (15)))) #f)
(check-equal? (comprueba-mas (hijos-arbol '(10 (7) (8)))) #t)

; RECURSION MUTUA
(define (ordenado-arbol? arbol)
  (and (and (comprueba (dato-arbol arbol)
                       (hijos-arbol arbol))
            (comprueba-mas (hijos-arbol arbol)))
       (ordenado-bosque? (hijos-arbol arbol))))

(define (ordenado-bosque? bosque)
  (cond
    ((null? bosque) #t)
    (else (and (ordenado-arbol? (car bosque))
               (ordenado-bosque? (cdr bosque))))))

(check-equal? (ordenado-arbol? '(10 (5) (7))) #t)
(check-equal? (ordenado-arbol? '(50 (10 (4) (6) (8)) (25 (15)))) #t)
(check-equal? (ordenado-arbol? '(10 (8) (7))) #f)
(check-equal? (ordenado-arbol? '(6 (5) (7))) #f)
(check-equal? (ordenado-arbol? '(50 (10 (4) (6) (11)) (25) (15))) #f)


;; ACTIVIDAD 4

;; (A)
(define arbol4A '(1 (1 (1)) (2 (3) (2)) (1 (2) (3))))

; AUXILIAR: incr 1 la izquierda de la pareja
(define (inc-izq p)
  (cons (+ 1 (car p))
        (cdr p)))
; AUXILIAR: incr 1 la derecha de la pareja
(define (inc-der p)
  (cons (car p)
        (+ 1 (cdr p))))
; AUXILIAR: suma la inz y der de dos parejas
(define (suma-pareja p1 p2)
  (cons (+ (car p1) (car p2))
        (+ (cdr p1) (cdr p2))))

(check-equal? (inc-izq (cons 5 6)) (cons 6 6))
(check-equal? (inc-der (cons 0 3)) (cons 0 4))
(check-equal? (suma-pareja (cons 1 2)
                           (cons 2 1)) (cons 3 3))

;(define (veces a b arbol)
(define (veces a b arbol)
  (cond
    ((= (dato-arbol arbol) a)
     (inc-izq (veces-bosque a b (hijos-arbol arbol))))
    ((= (dato-arbol arbol) b)
     (inc-der (veces-bosque a b (hijos-arbol arbol))))
    (else (veces-bosque a b (hijos-arbol arbol)))))

(define (veces-bosque a b bosque)
  (cond
    ((null? bosque) (cons 0 0))
    (else (suma-pareja (veces a b (car bosque))
                       (veces-bosque a b (cdr bosque))))))

(check-equal? (veces 1 2 arbol4A) '(4 . 3))
(check-equal? (veces 4 3 arbol4A) '(0 . 2))
(check-equal? (veces 4 5 arbol4A) '(0 . 0))

;; (B)
;Suponemos que lista contiene al menos un elemento
(define arbol4b '(a (a (a) (b)) (b (a) (c)) c))
;(es-camino? '(a b a) arbol) ⇒ #t
;(es-camino? '(a b) arbol) ⇒ #f
;(es-camino? '(a b a b) arbol) ⇒ #f

;; (C)
(define arbol4c '(1 (2 (3 (4) (2)) (5)) (6 (7))))

(define (nodos-nivel nivel arbol)
  (if (= nivel 0)
      (append (list (dato-arbol arbol))
              (nodos-bosque nivel (hijos-arbol arbol)))
      (nodos-bosque nivel (hijos-arbol arbol))))

(define (nodos-bosque nivel bosque)
  (cond
    ((null? bosque) '())
    (else (append (nodos-nivel (- nivel 1) (car bosque))
                  (nodos-bosque nivel (cdr bosque))))))

(check-equal? (nodos-nivel 0 arbol4c) '(1))
(check-equal? (nodos-nivel 1 arbol4c) '(2 6))
(check-equal? (nodos-nivel 2 arbol4c) '(3 5 7))
(check-equal? (nodos-nivel 3 arbol4c) '(4 2))

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