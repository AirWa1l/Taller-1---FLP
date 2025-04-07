#lang eopl
;******************************************************************************************
; Autores : Juan Fernando Calle Sanchez - 2127464 ;
;           Michael Ramirez Suriel - 2122236 ;
;           Jose Adrian Marin Ordoñez - 2126988 ;

; Implementación de representación de sintáxis abstracta de un circuito ;

;******************************************************************************************
;; Interpretador para lenguaje que permite evaluar expresiones matemáticas y circuitos lógicos combinacionales

;; La definición BNF para las expresiones del lenguaje:
;;
;;  <program>     ::= <expression>
;;  <expression>  ::= <number>
;;                  ::= <identifier>
;;                  ::= <primitive> ({<expression>}*(,))
;;                  ::= if <expresion> then <expresion> else <expresion>
;;                  ::= let (identifier = expression)* in expression
;;                  ::=<circuit>
;;  <primitive>     ::= + | - | * | add1 | sub1
;;                  ::=eval-circuit | connect-ciruits | merge-circuits
;;  <circuit>       ::=<gate_list>
;;  <gate-list>     ::= empty | <gate> <gate-list>
;;  <gate>          ::= <identifier> <type> <input-list>)
;;  <type>          ::= and | or | not | xor
;;  <input-list>    ::= empty | <bool> <input-list> | <identifier> <input-list>
;;  <bool>          ::= True | False

;******************************************************************************************

;Especificación Léxica

(define scanner-spec-simple-interpreter
  '((white-sp
      (whitespace) skip)
     (comment
      ("%" (arbno (not #\newline))) skip)
     (identifier
      (letter (arbno (or letter digit "?"))) symbol)
     (number
      (digit (arbno digit)) number)
     (number
      ("-" digit (arbno digit)) number)))

;Especificación Sintáctica (gramática)** Extensión de la gramática para incluir circuitos y operaciones con circuitos
(define grammar-simple-interpreter
  '((program (expression) a-program)
     (expression (number) lit-exp)
     (expression (identifier) var-exp)
     (expression ("'" identifier) symbol-exp)  ; Añade esta línea
     (expression (primitive "(" (separated-list expression ",") ")") primapp-exp)
     (expression ("if" expression "then" expression "else" expression) if-exp)
     (expression ("let" (arbno identifier "=" expression) "in" expression) let-exp)
     (expression (circuit) circuit-exp)
     (expression ("and") and-exp) ; Nueva regla para "and"
     (expression ("or") or-exp) ; Nueva regla para "or"
     (expression ("not") not-exp) ; Nueva regla para "not"
     (expression ("xor") xor-exp) ; Nueva regla para "xor"
     (expression ("True") true-exp)
     (expression ("False") false-exp)


     ; Primitivas
     (primitive ("+") add-prim)
     (primitive ("-") substract-prim)
     (primitive ("*") mult-prim)
     (primitive ("add1") incr-prim)
     (primitive ("sub1") decr-prim)
     (primitive ("eval-circuit") eval-circuit-prim)
     (primitive ("connect-circuits") connect-circuits-prim)
     (primitive ("merge-circuits") merge-circuits-prim)

     ; Circuitos
     (circuit ("(" "circuit" gate-list ")") a-circuit)
     (gate-list ("(" "gate_list" (arbno gate) ")") my-gate-list)
     (gate ("(" "gate" identifier gate-type input-list ")") a-gate)
     (gate-type ("and") and-type)
     (gate-type ("or") or-type)
     (gate-type ("not") not-type)
     (gate-type ("xor") xor-type)
     (input-list ("(" "input_list" (arbno input) ")") my-input-list)
     (input (boolean) bool-input)
     (input (identifier) ref-input)
     (boolean ("True") true-val)
     (boolean ("False") false-val)))

;Tipos de datos para la sintaxis abstracta de la gramática

(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

;*******************************************************************************************
;Parser, Scanner, Interfaz

(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))

(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program  pgm))
    (sllgen:make-stream-parser
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))


;*******************************************************************************************
;El Interprete

(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (body)
        (eval-expression body (init-env))))))

(define init-env
  (lambda ()
    (empty-env)))  ; Ambiente completamente vacío

(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (lit-exp (datum) datum)
      (var-exp (id) (apply-env env id))
      (symbol-exp (id) id)
      (and-exp () 'and)
      (or-exp () 'or)
      (not-exp () 'not)
      (xor-exp () 'xor)
      (true-exp () #t)
      (false-exp () #f)
      (primapp-exp (prim rands)
        (let ((args (eval-rands rands env)))
          (apply-primitive prim args env)))  ; Pasar el entorno a apply-primitive
      (if-exp (test-exp true-exp false-exp)
        (let ((test-val (eval-expression test-exp env)))
          (if (true-value? test-val)
              (eval-expression true-exp env)
              (eval-expression false-exp env))))
      (let-exp (ids rands body)
        (let ((args (eval-rands rands env)))
          (eval-expression body (extend-env ids args env))))
      (circuit-exp (circuit) circuit))))
(define extract-circuit-result
  (lambda (env)
    (cases environment env
      (empty-env-record ()
        0)
      (extended-env-record (syms vals parent-env)
        (if (null? syms)
            #f
            (car vals))))))
  ; Devuelve lista con el primer par (símbolo valor)

(define type-of
  (lambda (g)
    (cases gate g
      (a-gate (name type inputs) type))))

(define eval-rands
  (lambda (rands env)
    (map (lambda (x)
           (eval-rand x env))
         rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))


(define apply-primitive
  (lambda (prim args env)
    (cases primitive prim
      (add-prim () (+ (car args) (cadr args)))
      (substract-prim () (- (car args) (cadr args)))
      (mult-prim () (* (car args) (cadr args)))
      (incr-prim () (+ (car args) 1))
      (decr-prim () (- (car args) 1))
      (eval-circuit-prim ()
  (if (not (= (length args) 1))
      (eopl:error 'apply-primitive "eval-circuit requiere exactamente 1 argumento")
      (let ((circ (car args)))
        (if (circuit? circ)
              (let ((circuit-env (eval-circuit circ env))) ; Usar el entorno pasado
                circuit-env) ; Devolver el entorno directamente para inspección
            circ))))
       (connect-circuits-prim ()
        (if (= (length args) 3)
            (let ((circ1 (car args))
                  (circ2 (cadr args))
                  (target-gate-name (caddr args)))
              (if (and (circuit? circ1) (circuit? circ2) (symbol? target-gate-name))
                  (begin
                    (connect-circuits circ1 circ2 target-gate-name))
                  (eopl:error 'connect-circuits "Argumentos inválidos: se esperaban dos circuitos y un símbolo")))
            (eopl:error 'connect-circuits "Número incorrecto de argumentos: se esperaban exactamente 3, recibió ~a" (length args))))
      (merge-circuits-prim ()
        (if (not (= (length args) 4)) ; Debe recibir 4 argumentos
            (eopl:error 'apply-primitive "merge-circuits requiere exactamente 4 argumentos, recibió ~s" args)
            (let ((circ1 (car args))
                  (circ2 (cadr args))
                  (gate-type-sym (caddr args)) ; Recibe el tipo como símbolo
                  (new-gate-name (cadddr args)))
              (if (and (circuit? circ1) (circuit? circ2) (symbol? gate-type-sym) (symbol? new-gate-name))
                  (merge-circuits circ1 circ2 gate-type-sym new-gate-name)
                  (eopl:error 'apply-primitive "Argumentos inválidos para merge-circuits"))))))))

(define true-value?
  (lambda (x)
    (not (or (zero? x) (eq? x #f)))))

(define last
  (lambda (lst)
    (if (null? (cdr lst))
        (car lst)
        (last (cdr lst)))))

;*******************************************************************************************
;Ambientes

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
    (syms (list-of symbol?))
    (vals (list-of scheme-value?))
    (env environment?)))

(define scheme-value?
  (lambda (v)
    (or (number? v) (boolean? v) (circuit? v) (eq? v #f)(eq? v #t) ))) ; Asegura que #f sea reconocido

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

(define apply-env
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
        (eopl:error 'apply-env "No binding for ~s" sym))
      (extended-env-record (syms vals env)
        (let ((pos (list-find-position sym syms)))
          (if (number? pos)
              (let ((val (list-ref vals pos)))
                val)
              (apply-env env sym)))))))

; *************************************************************************************** ;
; Funciones Auxiliares ;
; *************************************************************************************** ;

; Función para extraer el nombre de una compuerta ;
(define (get-gate-name g)
  (cases gate g
    (a-gate (name type inputs) name)))

; Función para obtener el nombre de una compuerta ; 
(define gate-name
  (lambda (g)
    (cases gate g
      (a-gate (name type inputs) name))))

; Función para extraer el tipo de compuerta lógica "and, or, xor, not";
(define gate_type
  (lambda (g)
    (cases gate g
      (a-gate (name type inputs) type))))

; Extraemos la lista de entradas de una compuerta ;
(define gate-inputs
  (lambda (g)
    (cases gate g
      (a-gate (name type inputs) inputs))))

; Aplicamos la operación correspondiente según el tipo de compuerta ;
(define apply-gate
  (lambda (type inputs)
    (cases gate-type type
      (and-type () (and (car inputs) (cadr inputs)))
      (or-type () (or (car inputs) (cadr inputs)))
      (not-type () (not (car inputs)))
      (xor-type () (xor (car inputs) (cadr inputs))))))

; Función de apoyo para el XOR ;
(define xor
  (lambda (a b)
    (and (or a b) (not (and a b)))))

; Función auxiliar que reemplaza las referencias ;
(define replace-refs
  (lambda (inputs target-name new-name)
    (cases input-list inputs
      (my-input-list (input-list-val)
        (my-input-list
          (map (lambda (inp)
                 (cases input inp
                   (ref-input (old-name)
                     (if (eqv? old-name target-name)
                         (ref-input new-name) ; Reemplazar la referencia
                         inp)) ; Mantener la referencia original
                   (bool-input (val) inp))) ; Mantener la entrada booleana original
               input-list-val))))))

; Convertimos las cadenas de texto, en el tipo especificado de compuerta ;
(define symbol->gate-type
  (lambda (sym)
    (cond
      ((eq? sym 'and) (and-type))
      ((eq? sym 'or) (or-type))
      ((eq? sym 'not) (not-type))
      ((eq? sym 'xor) (xor-type))
      (else (eopl:error 'symbol->gate-type "Tipo de compuerta no válido: ~s" sym)))))

; --------------------------------------------------------------------------------------------------------------------------------------- ;
; Funciones Principales "Eval-circuit | connect-circuits | merge-circuits" ;
; --------------------------------------------------------------------------------------------------------------------------------------- ;

; Evaluamos un circuito completo. Se recibe un circuito y un ambiente ;
; de evaluación, y evaluamos el circuito en ese entorno ;
(define eval-circuit
  (lambda (circuit-val env)
    (cases circuit circuit-val
      (a-circuit (gates)
        (let ((final-env (eval-gate-list gates env)))
          ; Obtenemos específicamente el valor de la última compuerta evaluada (B)
          (let ((last-gate-name (gate-name (last (gate-list-gates gates)))))
            (apply-env final-env last-gate-name)))))))

; Función auxiliar para obtener la lista de compuertas
(define gate-list-gates
  (lambda (gates)
    (cases gate-list gates
      (my-gate-list (gate-structs) gate-structs))))
; Evaluamos una lista de compuertas de forma secuencial, al mismo tiempo ;
; vamos actualizando el entorno con cada uno de los resultados de las com ;
; puertas por medio de la recursión ;
(define eval-gate-list
  (lambda (gates env)
    (cases gate-list gates
      (my-gate-list (gate-structs)
        (if (null? gate-structs)
            env
            (let ((current-gate (car gate-structs)))
              (let ((gate-result (eval-gate current-gate env)))
                (let ((gate-name (car gate-result))
                      (gate-value (cadr gate-result)))
                  (eval-gate-list 
                   (my-gate-list (cdr gate-structs))
                   (extend-env (list gate-name) (list gate-value) env))))))))))
; Se evaluá una compuerta individual. Primero evalúa las entradas usando ;
; el entorno actual para posteriormente aplicar la operación lógica corres ;
; pondiente al tipo de compuerta para devolver una lista con el nombre y el ;
; valor obtenido ;
(define (eval-gate g env)
  (cases gate g
    (a-gate (name type input-list)
      (let ((input-values (eval-input-list input-list env)))
        (let ((result (apply-gate type input-values)))
          (list name result))))))

; Evaluamos la lista de entradas usando booleanos o una referencia a ;
; otra compuerta "en este caso se busca el valor correspondiente en el ;
; entorno ;
(define (eval-input-list input-list-val env)
  (cases input-list input-list-val
    (my-input-list (inputs)
      (map (lambda (inp)
             (cases input inp
               (bool-input (val)
                 (cases boolean val
                   (true-val () #t)
                   (false-val () #f)))
               (ref-input (name)
                 (apply-env env name))))
           inputs))))

; Conectamos dos circuitos, donde la salida del primer circuito alimenta ;
; una compuerta específica del segundo circuito ;
; Función mejorada para conectar circuitos
(define connect-circuits
  (lambda (circ1 circ2 target-input)
    (cases circuit circ1
      (a-circuit (gates1)
        (cases circuit circ2
          (a-circuit (gates2)
            (let ((output-gate (last-gate gates1)))
              (a-circuit
                (my-gate-list
                  (append
                    (gate-list-gates gates1) ; Mantenemos CIRC1 intacto
                    (update-gates-inputs      ; Modificamos solo inputs en CIRC2
                      (gate-list-gates gates2)
                      target-input
                      (gate-name output-gate))))))))))))

(define update-gates-inputs
  (lambda (gates target-input replacement)
    (map
      (lambda (g)
        (cases gate g
          (a-gate (name type inputs)
            (a-gate
              name
              type
              (update-input-list inputs target-input replacement)))))
      gates)))

(define update-input-list
  (lambda (inputs target replacement)
    (cases input-list inputs
      (my-input-list (input-vals)
        (my-input-list
          (map
            (lambda (inp)
              (cases input inp
                (ref-input (name)
                  (if (eqv? name target)
                      (ref-input replacement) ; Reemplaza C por G4
                      inp))
                (bool-input (val) inp)))
            input-vals))))))
;; Función para obtener la última compuerta de una lista
(define last-gate
  (lambda (gates)
    (cases gate-list gates
      (my-gate-list (gate-structs)
        (if (null? (cdr gate-structs))
            (car gate-structs)
            (last-gate (my-gate-list (cdr gate-structs))))))))


; Combinamos 2 circuitos, de modo que creamos una nueva compuerta que toma ;
; como entradas las salidas de ambos circuitos ;
(define merge-circuits
  (lambda (circ1 circ2 gate-type-symbol new-gate-name)
    (cases circuit circ1
      (a-circuit (gates1-struct)
        (cases circuit circ2
          (a-circuit (gates2-struct)
            (let* ((gates1 (cases gate-list gates1-struct (my-gate-list (gates) gates)))
                   (gates2 (cases gate-list gates2-struct (my-gate-list (gates) gates)))
                   (last-gate1 (last gates1))
                   (last-gate2 (last gates2))
                   (gate-type-val (symbol->gate-type gate-type-symbol))
                   (new-gate (a-gate new-gate-name gate-type-val (my-input-list (list (ref-input (gate-name last-gate1)) (ref-input (gate-name last-gate2)))))))
              (a-circuit (my-gate-list (append gates1 (append gates2 (list new-gate))))))))))))

; funcion que encuentra la posición de un simbolo dentro de una lista ;
(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

; Función auxiliar que encuentra el índice del primer elemento en una ;
; lista que satisface un predicado dado. Retorna el índice (comenzando desde 0) ;
; o #f si ningún elemento cumple con el predicado ;
(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                  (+ list-index-r 1)
                  #f))))))

; Convertimos un input-list en una lista tomando los elementos proporcionados ;
(define input-list->list
  (lambda (input-list-val)
    (cases input-list input-list-val
      (my-input-list (inputs) inputs))))

(interpretador)
;******************************************************************************************
;Pruebas
;*****Circuito con una compuerta NOT*********
;;let A = True
;;in let C1 = (circuit (gate_list (gate B not (input_list A))))
;;in eval-circuit (C1)---> Devuelve False
;****** Circuito AND simple*********
;;let A = True
;;    B = True in
;;let C2 = (circuit (gate_list 
;;                  (gate G1 and (input_list A B))))
;;in eval-circuit (C2)  ---> Devuelve True

;****** Circuito OR simple*********
;;let C3 = (circuit (gate_list(gate G1 or (input_list False True))))
;;  in eval-circuit (C3)  ---> Devuelve True

;*******Representacion del XOR****************
;; 1. A=True B=True
;;let C3 = (circuit (gate_list
;;                   (gate G1 or (input_list True True))
;;                   (gate G2 and (input_list True True)) 
;;                   (gate G3 not (input_list G2))
;;                   (gate G4 and (input_list G1 G3))))
;; in eval-circuit (C3)  -->false

;; 2. A=False B=False
;;let C3 = (circuit (gate_list
;;                   (gate G1 or (input_list False False))
;;                   (gate G2 and (input_list False False)) 
;;                   (gate G3 not (input_list G2))
;;                   (gate G4 and (input_list G1 G3))))
;; in eval-circuit (C3)  -->False

;; 3.
;; let A = True
;;     B = False in
;;let C3 = (circuit (gate_list
;;                  (gate G1 or (input_list True False))
;;                (gate G2 and (input_list True False)) 
;;                (gate G3 not (input_list G2))
;;                (gate G4 and (input_list G1 G3))))
;;in eval-circuit (C3)-->True

;;let C3 = (circuit (gate_list
;;                   (gate G1 and (input_list True True))
;;                   (gate G2 not (input_list G1))))
;; in eval-circuit (C3)  -->false
;;////////////////////////////////CONNECT-CIRCUITS////////////////////////
;;*****Conexion de los circuitos de la seccion 3.1 y 3.2*******************
;;let A=True B=True in 
;;let C1 = (circuit (gate_list
;;                    (gate G not (input_list A))))
;;C2 = (circuit (gate_list
;;                    (gate G1 and (input_list A B))))
;;  in connect-circuits (C1, C2, 'A)
;;**EVALUACION**
;;let A=True B=True in 
;;let C1 = (circuit (gate_list
;;                    (gate G not (input_list A))))
;;C2 = (circuit (gate_list
;;                    (gate G1 and (input_list A B))))
;;  in eval-circuit(connect-circuits (C1, C2, 'A))--> Devuelve False

;;*****Conexion de los circuitos de la seccion 3.1 y 3.3*******************
;;let A=True B=True in 
;;let C1 = (circuit (gate_list
;;                  (gate G not (input_list A))))
;;C2 = (circuit (gate_list
;;                (gate G1 or (input_list A B))
;;                (gate G2 and (input_list A B)) 
;;                (gate G3 not (input_list G2))
;;                (gate G4 and (input_list G1 G3))))
;;  in connect-circuits (C1, C2, 'A)
;;**EVALUACION**
;;let A=True B=True in 
;;let C1 = (circuit (gate_list
;;                  (gate G not (input_list A))))
;;C2 = (circuit (gate_list
;;                (gate G1 or (input_list A B))
;;                (gate G2 and (input_list A B)) 
;;                (gate G3 not (input_list G2))
;;                (gate G4 and (input_list G1 G3))))
;;  in eval-circuit(connect-circuits (C1, C2, 'A))-->True
;;*****Conexion de los circuitos de la seccion 3.2 y 3.3*******************
;;let A=False B=True in 
;;let C1 = (circuit (gate_list
;;                  (gate G and (input_list A B))))
;;C2 = (circuit (gate_list
;;                (gate G1 or (input_list A B))
;;                (gate G2 and (input_list A B)) 
;;                (gate G3 not (input_list G2))
;;                (gate G4 and (input_list G1 G3))))
;;  in connect-circuits (C1, C2, 'A)
;;**EVALUACION**
;;let A=False B=True in 
;;let C1 = (circuit (gate_list
;;                  (gate G and (input_list A B))))
;;C2 = (circuit (gate_list
;;                (gate G1 or (input_list A B))
;;                (gate G2 and (input_list A B)) 
;;                (gate G3 not (input_list G2))
;;                (gate G4 and (input_list G1 G3))))
;;  in eval-circuit(connect-circuits (C1, C2, 'A))


;;/////////////////////////MERGE-CIRCUITS////////////////////////

;;*****Conexion de los circuitos de la seccion 3.1 y 3.2 con and*******************

;;let A=False B=True C=True in 
;;let C1 = (circuit (gate_list
;;                    (gate G not (input_list A))))
;;C2 = (circuit (gate_list
;;                    (gate G1 and (input_list B C))))
;;  in merge-circuits (C1, C2 , and , 'G10)
;;**EVALUACION**
;;let A=False B=True C=True in 
;;let C1 = (circuit (gate_list
;;                    (gate G not (input_list A))))
;;C2 = (circuit (gate_list
;;                    (gate G1 and (input_list B C))))
;;  in eval-circuit(merge-circuits (C1, C2 , and , 'G10))-->True

;;*****Conexion de los circuitos de la seccion 3.1 y 3.3 con or*******************
;;let A=True B=True in 
;;let C1 = (circuit (gate_list
;;                  (gate G not (input_list A))))
;;C2 = (circuit (gate_list
;;                (gate G1 or (input_list A B))
;;                (gate G2 and (input_list A B)) 
;;                (gate G3 not (input_list G2))
;;                (gate G4 and (input_list G1 G3))))
;;  in  merge-circuits (C1, C2 , and , 'G20)
;;**EVALUACION**
;;let A=True B=True in 
;;let C1 = (circuit (gate_list
;;                  (gate G not (input_list A))))
;;C2 = (circuit (gate_list
;;                (gate G1 or (input_list A B))
;;                (gate G2 and (input_list A B)) 
;;                (gate G3 not (input_list G2))
;;                (gate G4 and (input_list G1 G3))))
;;  in eval-circuit(merge-circuits (C1, C2 , or , 'G20))-->False

;;*****Conexion de los circuitos 3.1 y 3.2 xor 3.1 y 3.3*******************
;;      let A = False B = True C = True
;;C1 = (circuit (gate_list
;;                          (gate G_1 not (input_list A))))
;;          C2 = (circuit (gate_list
;;                          (gate G1_1 and (input_list B C))))
;;       MERGE_1= merge-circuits(C1, C2, and, 'G10)
;;  
;;       D = True E = True
;     C3 = (circuit (gate_list
;;                          (gate G not (input_list D))))
;;          C4 = (circuit (gate_list
;;                          (gate G1 or (input_list D E))
;;                          (gate G2 and (input_list D E))
;;                          (gate G3 not (input_list G2))
;;                          (gate G4 and (input_list G1 G3))))
;;       MERGE_2=merge-circuits(C3, C4, and, 'G20)

;;in merge-circuits(MERGE_1, MERGE_2, xor, 'G100)
