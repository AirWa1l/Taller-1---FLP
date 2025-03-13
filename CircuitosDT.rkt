#lang eopl
; Autores: Juan Fernando Calle Sanchez - 2127464
;          Michael Ramirez Suriel - 202122236
;          Jose Adrian Marin Ordoñez - 2126988
; Implementación de circuitos lógicos usando datatypes en Racket

; Definición del tipo de dato circuito
; <circuit> ::= '(circuit <gate-list>)
(define-datatype circuit circuit?
  (a-circuit (gate_list gate_list?)))

; Definición del tipo de dato lista de compuertas
; <gate-list> ::= (empty) | (<gate> <gate-list>)
(define-datatype gate_list gate_list?
  (empty-gate-list)
  (no-empty-gate-list (first gate?) (rest gate_list?)))

; Definición del tipo de dato compuerta lógica
; <gate> ::= '(gate <gate_id> <type> <input_list>)
; <gate_id> ::= <symbol>
(define-datatype gate gate?
  (a-gate (gate_id symbol?) (gate_type type?) (input_list input_list?)))

; Definición de tipos de compuertas lógicas
; Representa los diferentes tipos de operaciones lógicas que puede realizar una compuerta.
; <type> ::= and | or | not | xor
(define-datatype type type?
  (and)
  (or)
  (not)
  (xor))

; Definición de lista de entradas (input_list)
; <input_list> ::= empty | <bool> <input_list> | <gate_ref> <input_list>
; <gate_ref> ::= <symbol>
(define-datatype input_list input_list?
  (empty-input-list)
  (no-empty-bool-input-list (first boolean?) (rest input_list?))
  (no-empty-gate-ref (first symbol?) (rest input_list?)))

; -------------------------------------------------------------------------
; EJEMPLOS DE CIRCUITOS
; -------------------------------------------------------------------------

; Ejemplo 1: Circuito con una compuerta NOT
; A -> (NOT A) -> SALIDA(G1)
(a-circuit
        (no-empty-gate-list 
        (a-gate 'G1 (not) (no-empty-gate-ref 'A (empty-input-list))) 
        (empty-gate-list)))

; Ejemplo 2: Circuito AND simple
; (A AND B) -> SALIDA (G2)         
(a-circuit
        (no-empty-gate-list 
        (a-gate 'G2 (and) (no-empty-gate-ref 'A (no-empty-gate-ref 'B (empty-input-list))))  
        (empty-gate-list)))
; Ejemplo 3: Combinación de compuertas
; (A OR B) -> G3 -> (NOT G3) -> SALIDA (G4)
(a-circuit
        (no-empty-gate-list 
        (a-gate 'G3 (or) (no-empty-gate-ref 'A (no-empty-gate-ref 'B (empty-input-list))))  
        (no-empty-gate-list
        (a-gate 'G4 (not) (no-empty-gate-ref 'G3 (empty-input-list)))
        (empty-gate-list))))

; Ejemplo 4: Implementación de XOR sin compuerta XOR
; (A OR B) -> G1
; (A AND B) -> G2 -> (NOT G2) -> G3
; (G1 AND G3) -> G4 -> SALIDA
(a-circuit
        (no-empty-gate-list 
        (a-gate 'G1 (or) (no-empty-gate-ref 'A (no-empty-gate-ref 'B (empty-input-list))))  
        (no-empty-gate-list
        (a-gate 'G2 (and) (no-empty-gate-ref 'A (no-empty-gate-ref 'B (empty-input-list))))
        (no-empty-gate-list
        (a-gate 'G3 (not) (no-empty-gate-ref 'G2 (empty-input-list)))
        (no-empty-gate-list
        (a-gate 'G4 (and) (no-empty-gate-ref 'G1 (no-empty-gate-ref 'G3 (empty-input-list))))
        (empty-gate-list))))))
