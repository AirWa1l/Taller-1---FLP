#lang eopl
; Autores : Juan Fernando Calle Sanchez - 2127464 ;
;           Michael Ramirez Suriel - 2122236 ;
;           Jose Adrian Marin Ordoñez - 2126988 ;
; Implementación de representación de sintáxis abstracta ;
; de un circuito ;

; Definición de estructuras de datos ;

; Definición del tipo de dato circuito
; <circuit> ::= '(circuit <gate-list>)
(define-datatype circuit circuit?
  (a-circuit (gate_list gate?)))

; Definición del tipo de dato compuerta lógica ;
; <gate> ::= '(gate <name> <gate-type> <inputs>) ;
; <name ::= <symbol>
(define-datatype gate gate?
  (a-gate (name symbol?) (type gate-type?) (inputs list?)))

; Definición del tipo de compuerta lógica ;
; <gate-type> ::= 'not | 'and | 'or
(define-datatype gate-type gate-type?
  (not-type)
  (and-type)
  (or-type)
  (xor-type))

; Definición de entradas de las compuertas lógicas ;
; <a-input> ::= <bool-input> | <ref-input> ;
; <bool-input> ::= <boolean> ;
; <ref-input> ::= <symbol>
(define-datatype a-input a-input?
  (bool-input (val boolean?))
  (ref-input (name symbol?)))

; --------------------------------------------------------------------
; Funciones PARSEBNF y UNPARSEBNF ;
; --------------------------------------------------------------------
; PARSEBNF ;
; -------------------------------------------------------------------- ;

; Inicialmente, verificamos si la lista lst representa un circuito válido ;
; mirando si este comienza con la palabra circuit, si es válido ;
; obtendremos la primera compuerta lógica de la lista usando parse- ;
; first-gate, y si no, devolvemos una compuerta de not-type sin entradas ;
(define (PARSEBNF lst)
  (if (and (pair? lst) (eq? (car lst) 'circuit))
      (a-circuit (parse-first-gate (cadr lst)))
      (eopl:error 'parse-circuit "Formato incorrecto de circuito: ~s" lst)))

; En este caso, recibimos una lista de compuertas glist, y comprobamos ;
; si esta lista comienza con la palabra clave gate_list, la cual no ;
; tomaremos en cuenta y buscará la primera compuerta válida de la lista ;
; usando parse-gate ;
(define (parse-first-gate glist)
  (if (and (pair? glist) (eq? (car glist) 'gate_list))
      (parse-first-gate (cdr glist))
      (parse-gate (car glist))))

; Aqui, verificamos si g es efectivamente una compuerta valida y esto ;
; lo comprobamos pues comenzará por la palabra gate, posteriormente ;
; extraerá el nombre de la compuerta, el tipo y por último la lista de ;
; entradas, si g no es válida devolveremos un not-type sin entradas ;
(define (parse-gate g)
  (if (and (pair? g) (eq? (car g) 'gate))
      (a-gate (cadr g) (parse-gate-type (cadr(caddr g))) (parse-input-list (cadddr g)))
      (a-gate 'unknown (not-type) '())))

; Aquí traduciremos el tipo de compuerta gt a una estructura que ;
; definimos, en este caso not, and y or, tiene el mismo not-type que ;
; casos anteriores ;
(define (parse-gate-type gt)
  (cond
    [(eq? gt 'not) (not-type)]
    [(eq? gt 'and) (and-type)]
    [(eq? gt 'or) (or-type)]
    [(eq? gt 'xor) (xor-type)]
    [else (eopl:error 'parse-gate-type 
                      "Tipo de operación no válido en la gramática: ~s" gt)]))


; Convertiremos una lista de entradas ilist en la estructura que ;
; definimos, en caso de que sea vacía nos devolverá una lista vacia ;
; pero si esta tiene elementos, sencillamente convertimos cada ;
; entrada con parse-input y la almacenaremos en una nueva lista ;
(define (parse-input-list ilist)
  (cond
    [(null? ilist) '()] 
    [(eq? (car ilist) 'input_list) (parse-input-list (cdr ilist))]
    [else (cons (parse-input (car ilist)) (parse-input-list (cdr ilist)))]))

; Por último, convertiremos un valor de entrada inp en el formato ;
; definido, y miramos los casos, en caso de que sea un booleano ;
; lo alamcenaremos en un bool-input, si es un simbolo en un ;
; ref-input y si es un valor no válido "y para evitar que estalle el ;
; programa" lo tratamos como un bool-input con valor #f por defecto ;
(define (parse-input inp)
  (cond
    [(boolean? inp) (bool-input inp)]
    [(symbol? inp) (ref-input inp)]
    [else (bool-input #f)]))
