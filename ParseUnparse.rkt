#lang eopl
; Autores : Juan Fernando Calle Sanchez - 2127464 ;
;           Michael Ramirez Suriel - 2122236 ;
;           Jose Adrian Marin Ordoñez - 2126988 ;
; Implementación de representación de sintáxis abstracta ;
; de un circuito ;

; Definición de estructuras de datos ;

; Definicion dle tipo de dato del circuito ;
; <circuit> ::= '(circuit <gate>) ;
(define-datatype circuit circuit?
  (a-circuit (gate gate?)))

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
  (or-type))

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
