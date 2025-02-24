#lang eopl
; Taller 1 - FLP;
(define multi
  (lambda ( x y )(if (> x y)(* x y )(- x y )))
 )
; Esto solo es una muestra ;

; 3. list-set ;
(define (list-set L n x)
  (cond
    ; Primer caso, sucede cuando le pasamos una lista vacia ;
    [(eq? L '()) '()]
    ; Segundo caso, reemplaza el primer elemento cuando n = 0 ;
    [(= n 0) (cons x(cdr L))]
    ; Tercer caso, recorre el resto de la lista ;
    [(> n 0) (cons(car L) (list-set(cdr L) (- n 1) x))]
    )
  )