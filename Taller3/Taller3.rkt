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

; -------------------------------------------------------- ;
; Implementación para guardado de valores de variables     ;
; -------------------------------------------------------- ;
; Se define un environment que reprensenta un entorno vacio, y un  ;
; entorno extendido donde se agregara un var con un valor booleano ;
; a un entorno existente ;
(define-datatype environment environment?
  (empty-env)
  (extend-env (var symbol?) (val boolean?) (env environment?)))

; Buscamos una variable en un entorno, si la encuentra devuelve el ;
; valor, sino sigue buscando en un entorno anterior y si llega a ;
; un entorno vacio lanza un error ;
(define (apply-env env search-var)
  (cases environment env
    (empty-env () (eopl:error 'apply-env "Error: No hay vinculación para la variable" search-var))
    (extend-env (saved-var saved-val saved-env)
                (if (eqv? saved-var search-var)
                    saved-val
                    (apply-env saved-env search-var))
                )
    )
  )

; Función de apoyo que devuelve el último elemento de la lista ;
(define (last lst)
  (if (null? (cdr lst))
      (car lst)
      (last (cdr lst))))

; Función que aplica las operaciones lógicas dependiendo de las puertas ;
; lógicas definidas ;
(define (apply-gate type inputs)
  (cases gate-type type
    (not-type () (not (car inputs)))
    (and-type () (and (car inputs) (cadr inputs)))
    (or-type () (or (car inputs) (cadr inputs)))
    (xor-type () (xor (car inputs) (cadr inputs)))))

; función de apoyo para XOR ;
(define (xor a b)
  (or (and a (not b)) (and (not a) b)))

; ---------------------------------------------------------- ;
; Eval-Circuit                                               ;
; ---------------------------------------------------------- ;
(define (eval-circuit circuit-val env)
  (cases circuit circuit-val
    (a-circuit (gates)
      (let ((new-env (eval-gate-list gates env)))
        (cases gate (last gates)
          (a-gate (name type inputs)
            (apply-env new-env name)))))))

(define (eval-gate-list gates env)
  (if (null? gates)
      env
      (let* ((current-gate (car gates))
             (gate-result (eval-gate current-gate env))
             (new-env (extend-env (get-gate-name current-gate) gate-result env)))
        (eval-gate-list (cdr gates) new-env))))

(define (get-gate-name g)
  (cases gate g
    (a-gate (name type inputs) name))
  )

(define (eval-gate g env)
  (define (eval-input-list input-list env)
    (if (null? input-list) '()
        (cons (eval-input (car input-list) env)(eval-input-list (cdr input-list) env))))
  (cases gate g
    (a-gate (name type inputs)
            (let ((input-values (eval-input-list inputs env)))
              (apply-gate type input-values)))))

(define (eval-input inp env)
  (cases a-input inp
    (bool-input (val) val)
    (ref-input (name) (apply-env env name))))

; ----------------------------------------------------------------------------------------------------- ;
; Pruebas ;
; ----------------------------------------------------------------------------------------------------- ;
(define test-env 
  (extend-env 'A #t
    (empty-env)))

(define test-circuit 
  (PARSEBNF '(circuit
              (gate_list
               (gate G1 (type not) (input_list A))))))

(eval-circuit test-circuit test-env)
; ------------------------------------------------------------------------------------------------------- ;
(define test2-env 
  (extend-env 'A #t
    (extend-env 'B #f
      (empty-env))))

(define test-circuit2 
  (PARSEBNF '(circuit
              (gate_list
               (gate G1 (type or) (input_list A B))
               (gate G2 (type and) (input_list A B))
               (gate G3 (type not) (input_list G2))
               (gate G4 (type and) (input_list G1 G3))))))

(eval-circuit test-circuit2 test2-env)

; ----------------------------------------------------------------------- ;
(define test3-env 
  (extend-env 'A #t
    (extend-env 'B #t
      (empty-env))))

(define test-circuit3 
  (PARSEBNF '(circuit
              (gate_list
               (gate G2 (type and) (input_list A B))))))

(eval-circuit test-circuit3 test3-env)





