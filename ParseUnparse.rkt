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
  (and−type)
  (or−type)
  (not−type)
  (xor−type))

; Definición de lista de entradas (input_list)
; <input_list> ::= empty | <bool> <input_list> | <gate_ref> <input_list>
; <gate_ref> ::= <symbol>
(define-datatype input_list input_list?
  (empty-input-list)
  (no-empty-bool-input-list (first boolean?) (rest input_list?))
  (no-empty-gate-ref (first symbol?) (rest input_list?)))

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
      (a-gate (cadr g) (parse-gate-type (caddr g)) (parse-input-list (cadddr g)))
      (a-gate 'unknown (not-type) '())))

; Aquí traduciremos el tipo de compuerta gt a una estructura que ;
; definimos, en este caso not, and y or, tiene el mismo not-type que ;
; casos anteriores ;
(define (parse-gate-type gt)
  (cond
    [(eq? gt 'not) (not-type)]
    [(eq? gt 'and) (and-type)]
    [(eq? gt 'or) (or-type)]
    [else (not-type)]))

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
; ----------------------------------------- ;
; UNPARSEBNF
; ----------------------------------------- ;

; Inicialmente, recibimos una instancia a la que llamaremos circuit- :
; parse y usamos cases para verificar si este es de tipo a-circuit ;
; y extraemos el campo gate del circuito y lo convertimos en un lista ;
; con la estructura circuit compuertas, por último llamamos a ;
; unparse-gate para convertir la compuerta ;
(define (UNPARSEBNF circuit-parse)
  (cases circuit circuit-parse
    (a-circuit (gate) (list 'circuit (unparse-gate gate)))))

; Aqui sencillamente recibimos la compuerta g y la evaluamos con cases ;
; verificando que sea de tipo a-gate, si lo es extraemos name, type ;
; e inputs y devolvemos una lista, llamaos a unparse-gate-type y ;
; a unparse-input-list para convertir los datos correspondientes ;
(define (unparse-gate g)
  (cases gate g
    (a-gate (name type inputs)
      (list 'gate
            name
            (unparse-gate-type type)
            (list 'input_list (unparse-input-list inputs))))))

; En esta función recibimos gt que representa el tipo de compuerta ;
; comparamos y devolvemos o un not, and o or dependiendo del caso ;
(define (unparse-gate-type gt)
  (cond
    [(eq? gt not-type) 'not]
    [(eq? gt and-type) 'and]
    [(eq? gt or-type) 'or]
    [else 'not])) ;; Caso por defecto en caso de error

; Convertiremos una lista de entradas ilist en la estructura que ;
; definimos, en caso de que sea vacía nos devolverá una lista vacia ;
; pero si esta tiene elementos, tomamos el 1er elemento y le aplicamos ;
; unparse-input, se agregar a una nueva lista y llamamos a unparse- ;
; input-list y hacemos lo mismo con el resto de la lista ;
(define (unparse-input-list ilist)
  (if (null? ilist) '()  
      (cons (unparse-input (car ilist)) 
            (unparse-input-list (cdr ilist)))))

; Finalmente, recibirmos un inp que es una entrada de la compuerta ;
; Ysamos los cases para identificar los 2 tipos de entreda y dependdiendo ;
; del caso devolvemos val o name "
(define (unparse-input inp)
  (cases a-input inp
    (bool-input (val) val)
    (ref-input (name) name)))
