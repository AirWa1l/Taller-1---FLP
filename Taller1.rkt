#lang eopl

;* PUNTO 2
;; Recibe una lista `L` y devuelve una nueva lista donde cada elemento de `L`
;; es colocado dentro de una lista adicional, aumentando su nivel de paréntesis.

(define down 
  (lambda (L) 
    (if (null? L)  ;; Si la lista está vacía, retornamos una lista vacía
        '()       
        (cons (list (car L))  ;; Colocamos el primer elemento en una nueva lista
              (down (cdr L))))))  ;; Llamada recursiva para el resto de la lista


;;Casos de prueba 
;;(down '(1 2 3))
;;(down '((una) (buena) (idea)))
;;(down '(un (objeto (mas)) complicado))
;;(down '(a 42 "hello" #t))
;;(down '((1 2) (3 (4 5)) 6))


;;Punto 5
;;List-index es una función que recibe un predicado y una lista, esta función devuelve

(define list-index
  (lambda (P L)
    (define aux
      (lambda (L acc)
        (cond
          ((null? L) #f)           ;; Si la lista está vacía, retornamos #f
          ((P (car L)) acc)        ;; Si el predicado se cumple, devolvemos el contador
          (else (aux (cdr L) (+ acc 1)))))) ;; Incrementamos el contador
    (aux L 0)))  ;; Iniciamos con el contador en 0

;;Casos de prueba
;;(list-index number? '(a 2 (1 3) b 7))
;;(list-index symbol? '(a (b c) 17 foo))
;;(list-index symbol? '(1 2 (a b) 3))

;;Punto 8

;; mapping: (X -> Y) Lista Lista -> Lista
;; Recibe una función `F`, dos listas `L1` y `L2`, y devuelve una lista de pares
;; donde `F` aplicado a un elemento de `L1` es equivalente al elemento correspondiente de `L2`.

(define mapping
  (lambda (F L1 L2)
    (cond
      ((or (null? L1) (null? L2)) '())  ;; Si alguna lista está vacía, se retorna lista vacía
      ((eqv? (F (car L1)) (car L2))  
       (cons (list (car L1) (car L2))  ;; Si coinciden, agregamos el par a la lista
             (mapping F (cdr L1) (cdr L2))))  ;; Llamada recursiva con el resto
      (else (mapping F (cdr L1) (cdr L2))))))  ;; Si no coinciden avanzamos en ambas listas
;;Casos de prueba
;;(mapping (lambda (d) (* d 2)) (list 1 2 3) (list 2 4 6))
;;(mapping (lambda (d) (* d 3)) (list 1 2 2) (list 2 4 6))
;;(mapping (lambda (d) (* d 2)) (list 1 2 3) (list 3 9 12))

;;Punto 11
;; Recibe una función F y dos listas L1 y L2, y devuelve una nueva lista 
;; aplicando F a cada par de elementos correspondientes de L1 y L2

(define zip
  (lambda (F L1 L2)
    (if (or (null? L1) (null? L2))  ;; Si alguna de las listas está vacía no continua el código 
        '()
        (cons (F (car L1) (car L2))  ;;Aplicamos el else, donde se aplica F al primer par de elementos
              (zip F (cdr L1) (cdr L2))))))  ;; Llamada recursiva con el resto de los elementos


;;Casos de prueba
;;(zip + '(1 4) '(6 2))
;;(zip * '(11 5 6) '(10 9 8))

;;Punto 14
;; Recibe un número n y un Árbol Binario de Búsqueda (`BST`).
;; Retorna una lista de símbolos left o right que representan el camino
;; para encontrar n en el árbol.

(define path
  (lambda (n BST)
    (if (null? BST)  ;; Si el árbol está vacío entonces devolvemos una lista vacía
        '()
        (cond 
          ((= n (car BST)) '())  ;; Si se encuentra el numero en el arbol entonces devolvemos una lista vacía
          ((< n (car BST))  
           (cons 'left (path n (cadr BST))))  ;; Si n es menor bajamos por la izquierda
          ((> n (car BST))  
           (cons 'right (path n (caddr BST))))))))  ;; Si n es mayor, bajamos por la derecha


;;Casos de prueba
;;(path 17 '(14 (7 () (12 () ()))(26 (20 (17 () ())())(31 () ()))))
;;(path 2 '(2 '() '()))


;;Punto 17
;; Recibe una matriz mat y un vector vec que devuelve una nueva matriz
;; donde cada elemento de una fila de mat ha sido multiplicado escalarmente
;; por el elemento correspondiente del vec.

(define prod-scalar-matriz
  (lambda (mat vec)
    (if (null? mat)  ;; Si la matriz está vacía, retornamos una lista vacía
        '()
        (cons (map * (car mat) vec)  ;; Multiplicamos fila por fila con el vector
              (prod-scalar-matriz (cdr mat) vec)))))  ;; Recurrimos con la siguiente fila
