#lang eopl

; Autores : Juan Fernando Calle Sanchez - 2127464 ;
;           Michael Ramirez Suriel - 2122236 ;
;           Jose Adrian Marin Ordoñez - 2126988 ;
; Implementación de representación de sintáxis abstracta ;
; de un circuito ;

; Definición de estructuras de datos ;

; Definición del tipo de dato circuito
(define-datatype circuit circuit?
  (a-circuit (glist (list-of gate?)))) ;; Asegurar que glist es una lista de compuertas

; Definición del tipo de dato compuerta lógica ;
(define-datatype gate gate?
  (a-gate (name symbol?) (type gate-type?) (inputs (list-of a-input?))))

; Definición del tipo de compuerta lógica ;
(define-datatype gate-type gate-type?
  (not-type)
  (and-type)
  (or-type)
  (xor-type))

; Definición de entradas de las compuertas lógicas ;
(define-datatype a-input a-input?
  (bool-input (val boolean?))
  (ref-input (name symbol?)))

; --------------------------------------------------------------------
; Funciones PARSEBNF y UNPARSEBNF ;
; --------------------------------------------------------------------

(define (PARSEBNF lst)
  (if (and (pair? lst) (eq? (car lst) 'circuit))
      (a-circuit (parse-gate-list (cadr lst)))
      (eopl:error 'PARSEBNF "Formato incorrecto de circuito: ~s" lst)))

(define (parse-gate-list glist)
  (if (and (pair? glist) (eq? (car glist) 'gate_list))
      (parse-gate-list-helper (cdr glist)) 
      (eopl:error 'parse-gate-list "Formato incorrecto para gate_list: ~s" glist)))

(define (parse-gate-list-helper gates)
  (if (null? gates)
      '() ;; Retorna una lista vacía
      (cons (parse-gate (car gates)) (parse-gate-list-helper (cdr gates))))) 

(define (parse-gate g)
  (if (and (pair? g) (eq? (car g) 'gate))
      (a-gate (cadr g) (parse-gate-type (cadr (caddr g))) (parse-input-list (cadddr g)))
      (eopl:error 'parse-gate "Formato incorrecto de compuerta: ~s" g)))

(define (parse-gate-type gt)
  (case gt
    [(not) (not-type)]
    [(and) (and-type)]
    [(or) (or-type)]
    [(xor) (xor-type)]
    [else (eopl:error 'parse-gate-type "Tipo de operación no válido: ~s" gt)]))

(define (parse-input-list ilist)
  (cond
    [(null? ilist) '()] 
    [(eq? (car ilist) 'input_list) (parse-input-list (cdr ilist))]
    [else (cons (parse-input (car ilist)) (parse-input-list (cdr ilist)))]))

(define (parse-input inp)
  (cond
    [(boolean? inp) (bool-input inp)]
    [(symbol? inp) (ref-input inp)]
    [else (bool-input #f)]))
; ----------------------------------------- ;
; UNPARSEBNF
; ----------------------------------------- ;
; Inicialmente, recibimos una instancia a la que llamaremos circuit- :
; parse y usamos cases para verificar si este es de tipo a-circuit ;
; y extraemos el campo gate-list y lo procesamos del circuito y lo convertimos en un lista ;
; con la estructura circuit gate_list compuertas, por último llamamos a ;
; unparse-gate-list para convertir la lista de compuertas ;
(define (UNPARSEBNF circuit-parse)
  (cases circuit circuit-parse
    (a-circuit (gate-list)
      (list 'circuit (cons 'gate_list (unparse-gate-list gate-list)))))) 

; Procesa una lista de compuertas y las convierte a su formato de salida
(define (unparse-gate-list gates)
  (if (null? gates)
      '()
      (cons (unparse-gate (car gates)) (unparse-gate-list (cdr gates))))) ;; Recorre recursivamente la lista de compuertas

;; Procesa una sola compuerta
(define (unparse-gate g)
  (cases gate g
    (a-gate (name type inputs)
      (list 'gate
            name
            (unparse-gate-type type)
            (cons 'input_list (unparse-input-list inputs))))))

; Convierte el tipo de compuerta
(define (unparse-gate-type gt)
  (cases gate-type gt
    (not-type () '(type not))
    (and-type () '(type and))
    (or-type () '(type or))
    (xor-type () '(type xor))
    (else '(type ERROR)))) ;; Retorna un error si no reconoce el tipo

;; Convierte una lista de entradas
(define (unparse-input-list ilist)
  (if (null? ilist) '()
      (cons (unparse-input (car ilist)) 
            (unparse-input-list (cdr ilist)))))

;; Convierte una entrada individual
(define (unparse-input inp)
  (cases a-input inp
    (bool-input (val) val)
    (ref-input (name) name)))

; --------------------------------------------------------------------
; Pruebas ;
; -------------------------------------------------------------------- 
; Ejemplo 1: Circuito con una compuerta NOT
; A -> (NOT A) -> SALIDA(G1)
(PARSEBNF '(circuit
                  (gate_list 
                  (gate G1 (type not) (input_list A )))))
(UNPARSEBNF(PARSEBNF '(circuit
                  (gate_list 
                  (gate G1 (type not) (input_list A ))))))
; Ejemplo 2: Circuito AND simple
; (A AND B) -> SALIDA (G2)         
(PARSEBNF '(circuit
                  (gate_list 
                  (gate G2 (type and) (input_list A B)))))

(UNPARSEBNF (PARSEBNF '(circuit
                  (gate_list 
                  (gate G2 (type and) (input_list A B))))))         
; Ejemplo 3: Combinación de compuertas
; (A OR B) -> G3 -> (NOT G3) -> SALIDA (G4)
(PARSEBNF '(circuit
                  (gate_list 
                  (gate G3 (type or) (input_list A B))
                  (gate G4 (type not) (input_list G3)))))

(UNPARSEBNF (PARSEBNF '(circuit
                               (gate_list 
                               (gate G3 (type or) (input_list A B))
                               (gate G4 (type not) (input_list G3))))))
; Ejemplo 4: Implementación de XOR sin compuerta XOR
; (A OR B) -> G1
; (A AND B) -> G2 -> (NOT G2) -> G3
; (G1 AND G3) -> G4 -> SALIDA
(UNPARSEBNF (PARSEBNF 
'(circuit
        (gate_list 
        (gate G1 (type or) (input_list A B))
        (gate G2 (type or) (input_list A B #f))
        (gate G4 (type or) (input_list A #t))
        (gate G1 (type or) (input_list #t G4))))))
