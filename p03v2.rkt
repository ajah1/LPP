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


;---------------------------------- Actividad 4

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


(define (cuenta-resultados r lr)
  (cond
    ((null? lr) 0)
    ((equal? (resultado-partido (car lr)) r)
     (+ 1 (cuenta-resultados r (cdr lr))))
    (else (cuenta-resultados r (cdr lr)))))


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


;---------------------------------- Actividad 5

(define (filtra-simbolos l-simb l-num)
  (cond
    ((null? l-simb) '())
    ((= (car l-num) (string-length (symbol->string (car l-simb))))
     (cons (cons (car l-simb) (car l-num))
           (filtra-simbolos (cdr l-simb) (cdr l-num))))
    (else (filtra-simbolos (cdr l-simb) (cdr l-num)))))

