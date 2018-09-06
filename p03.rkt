#lang r6rs
(import (rnrs base)
        (rnrs io simple)
        (schemeunit))

;---------------------------------- Actividad 1

(define (comprobar n v)
  (if (equal? (mod n v) 0) #t #f))

(define (multiplo-de n l)
  (cond
    ((null? l) '())
    ((comprobar n (car l)) (cons #t (multiplo-de n (cdr l))))
    (else (cons #f (multiplo-de n (cdr l))))))

;DEMOSTRACION
(display "\nActividad 1")
(display "\n(multiplo-de 10 '(100 23 10 300  48 7)) => ")
(display (multiplo-de 10 '(100 23 10 300  48 7)))
;PRUEBAS
(check-equal? (multiplo-de 6 '(3 23 10 300 6 7)) '(#t #f #f #f #t #f))



;---------------------------------- Actividad 2

; aux para devolver el procedure asociado
(define (mi-eval op)
  (cond
    ((equal? op '+) +)
    ((equal? op '-) -)
    ((equal? op '*) *)
    ((equal? op '/) /)))

; aux aplica el procedure a la pareja
(define (operar f p)
  (f (car p) (cdr p)))

(define (calcular-lista l)
  (cond
    ((null? l) '())
    (else (cons (operar (mi-eval (car l)) (cadr l))
                (calcular-lista (cddr l))))))

;DEMOSTRACION
(display "\n\nActividad 2")
(display "\n(calcular-lista '(+ (2 . 3) * (4 . 5))) => ")
(display (calcular-lista '(+ (2 . 3) * (4 . 5))))
; PRUEBAS
(check-equal? (calcular-lista '(/ (-6 . 2) - (4.5 . 0.5) + (2 . 3))) '(-3 4.0 5))



;---------------------------------- Actividad 3

; aux implementacion de append
(define (mi-append l1 l2)
  (cond
    ((null? l1) l2)
    (else (cons (car l1)
                (mi-append (cdr l1) l2)))))

; aux para decrementar la pareja
(define (dec-pareja p)
  (cons (car p)
        (- (cdr p) 1)))

(define (expande-pareja pareja)
  (cond
    ((equal? (cdr pareja) 0) '())
    (else (cons (car pareja)
                (expande-pareja (dec-pareja pareja))))))

(define (expande l)
  (cond
    ((null? l) '())
    (else (mi-append (expande-pareja (car l))
                     (expande (cdr l))))))
; DEMOSTRACION
(display "\n\nActividad 3")
(display "\n(expande '((#t . 3) (LPP . 2) (b . 4))) => ")
(display (expande '((#t . 3) ("LPP" . 2) (b . 4))))
; PRUEBAS
(check-equal? (expande '((a . 1) (b . 2) (c . 3))) '(a b b c c c))



;---------------------------------- Actividad 4

;;;;;;;;;;;;;;;;; 4.(A)
(define (resultado-partido partido)
  (cond
    ((> (car partido) (cdr partido)) 1)
    ((< (car partido) (cdr partido)) 2)
    (else 'X)))


(define (resultados-quiniela lp)
  (cond
    ((null? lp) '())
    (else (cons (resultado-partido (car lp))
                (resultados-quiniela (cdr lp))))))

;;;;;;;;;;;;;;;;; 4.(B)
(define (cuenta-resultados r lr)
  (cond
    ((null? lr) 0)
    ((equal? (resultado-partido (car lr)) r)
     (+ 1 (cuenta-resultados r (cdr lr))))
    (else (cuenta-resultados r (cdr lr)))))


;;;;;;;;;;;;;;;;; 4.(C)
; auxiliares para incrementar la lista
; que cuenta resultados
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


(define (cuenta-resultados-lista lr)
  (cond
    ((null? lr) '(0 0 0))
    
    ((equal? (resultado-partido (car lr)) 'X)
     (inc-empate (cuenta-resultados-lista (cdr lr))))
    
    ((equal? (resultado-partido (car lr)) 1)
     (inc-gana (cuenta-resultados-lista (cdr lr))))
    
    ((equal? (resultado-partido (car lr)) 2)
     (inc-pierde (cuenta-resultados-lista (cdr lr)))) ))

;DEMOSTRACION
(display "\n\nActividad 4")
(display "\n-(A)")
(display "\n(resultados-quiniela '((1 . 2) (4 . 4) (3 . 5) (6 . 2) (9 . 9))) => ")
(display (resultados-quiniela '((1 . 2) (4 . 4) (3 . 5) (6 . 2) (9 . 9))))
(display "\n-(B)")
(display "\n(cuenta-resultados 'X '((1 . 2) (4 . 4) (3 . 5) (6 . 2) (9 . 9))) => ")
(display (cuenta-resultados 'X '((1 . 2) (4 . 4) (3 . 5) (6 . 2) (9 . 9))))
(display "\n-(C)")
(display "\n(cuenta-resultados-lista '((1 . 2) (4 . 4) (3 . 5) (6 . 2) (9 . 9))) => ")
(display (cuenta-resultados-lista '((1 . 2) (4 . 4) (3 . 5) (6 . 2) (9 . 9))))

;PRUEBAS
;(A)
(check-equal? (resultados-quiniela '((2 . 2))) '(X))
(check-equal? (resultados-quiniela '((3 . 2) (4 . 3) (3 . 5))) '(1 1 2))
;(B)
(check-equal? (cuenta-resultados 1 '((1 . 2) (4 . 4) (3 . 5) (6 . 2) (9 . 9))) 1)
(check-equal? (cuenta-resultados 2 '((1 . 2) (4 . 4) (3 . 5) (6 . 2) (9 . 9))) 2)
;(C)
(check-equal? (cuenta-resultados-lista '((2 . 2))) '(0 1 0))
(check-equal? (cuenta-resultados-lista '((3 . 2) (4 . 3) (3 . 5))) '(2 0 1))



;---------------------------------- Actividad 5

(define (filtra-simbolos l-simb l-num)
  (cond
    ((null? l-simb) '())
    ((= (car l-num) (string-length (symbol->string (car l-simb))))
     (cons (cons (car l-simb) (car l-num))
           (filtra-simbolos (cdr l-simb) (cdr l-num))))
    (else (filtra-simbolos (cdr l-simb) (cdr l-num)))))

;DEMOSTRACION
(display "\n\nActividad 5")
(display "\n(filtra-simbolos '(este es un ejercicio de examen) '(2 1 2 9 1 6)) => ")
(display (filtra-simbolos '(este es un ejercicio de examen) '(2 1 2 9 1 6)))
