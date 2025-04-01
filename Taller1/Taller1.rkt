#lang eopl

; Taller 1 - FLP;

;; Funciones Auxiliares
;; my-append: Lista, Lista -> Lista
;; Uso: (my-append L1 L2) -> Lista de elementos de L1 y L2
;; Crea una unica lista con los elementos de 2 listas
(define my-append
  (lambda (L1 L2)
    (cond
      [(null? L1) L2]
      [(null? L2) L1]
      [else (cons (car L1) (my-append (cdr L1) L2 ))]
      )
    ))

;* PUNTO 1
;; invert: Lista x Predicado -> Lista
;; Uso: (invert L) = Lista, con pares ordenados (x, y)
;; pero cuando no hay lista vacia el primer par se invierte (y, x)
;; y la cola de la lista llama a la funcion recursivamente.
;; Gramatica
;; <Invert> ::= () 
;;         ::= (<Invert> <par_invertido>)
;; <par_invertido> ::= (<elemento> <elemento>)
(define (invert L)
  (if (null? L)
      '()  ;; Retorna lista vacía si L está vacía
      (cons (list (cadar L) (caar L)) (invert (cdr L))))) 

;;! Casos de prueba
;;(invert '((3 2) (4 2) (1 5) (2 8)))
;;(invert '(("HOLA" 9) (f 90) (82 7)))
;;(invert '((6 9) ((3 5 7) 18) (k 7)))

;* PUNTO 2
;; Recibe una lista `L` y devuelve una nueva lista donde cada elemento de `L`
;; es colocado dentro de una lista adicional, aumentando su nivel de paréntesis.
;; Coloca cada elemento de una lista dentro de una lista adicional.
;; <down> ::= (down <list>)
;; <list> ::= '() | (cons <element> <list>)
;; <element> ::= <atom> | <list>
;; <atom> ::= número | símbolo | string | booleano
;; <cases> ::= (cond 
;;                [(null? <list>) '()] 
;;                [else (cons (list (car <list>)) (down (cdr <list>)))])

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

; 3. list-set ;
; <list-set> ::= (<list><index><value>)
; <cases> ::= (cond 
;                [(eq? <list> '()) '()] ; 
;                [(= <index> 0) (cons <value> (cdr <list>))] ;
;                [(> <index> 0) (cons (car <list>) (list-set (cdr <list>) ; (- <index> 1) <value>))])
; <list> ::= '() | (cons <value> <list>) ;
; <index> ::= <integer> ;
; <value> ::= <any> ;


(define (list-set L n x)
  (cond
    ; Primer caso, sucede cuando le pasamos una lista vacia ;
    [(eq? L '()) '()]
    ; Segundo caso, reemplaza el primer elemento cuando n = 0 y se ;
    ; reemplaza a x por la posición 0 de la lista y se devuelve con ;
    ; el resto de la lista;
    [(= n 0) (cons x(cdr L))]
    ; Tercer caso, recorre el resto de la lista, de modo, aqui vamos ;
    ; obteniendo la cabeza y la recorremos hasta que n = 0 para llegar ;
    ; a la posición que deseamos cambiar, se llama de forma recursiva ;
    ; para cada elemento de la lista hasta que cumpla con alguno de los ;
    ; casos de parada para cualquier n > 0;
    [(> n 0) (cons(car L) (list-set(cdr L) (- n 1) x))]
    )
  )

; Casos de Prueba ;
; (list-set '(1 2 3 4) 3 '(a b c)) ;
; (list-set '(a b d e c) 1 '1) ;
; (list-set '(a b c d) 0 '(1 2)) ;

;* PUNTO 4

;;filter-in: Predicado x Lista -> Lista
;;Uso: (filter-in P L) = Lista que contiene los elementos
;;que pertenecen a L y satisfacen el predicado P mediante llamados recursivos
;;Gramatica
;;<filter-in> ::= () 
;;            ::= (filter-in <Predicado> <Lista>)
;;<Lista> ::= '()  
;;        ::= (<elemento> <Lista>)
(define filter-in
  (lambda (P L)
    (if (null? L)
        empty
        (if (P (car L)) (cons (car L) (filter-in P (cdr L) ) )
            (filter-in P (cdr L))
            )
        )
    )
  )

;;! Casos de prueba
;;(filter-in number? '(a 2 (1 3) b 7))
;;(filter-in symbol? '(a (b c) 17 foo))
;;(filter-in string? '(a b u "univalle" "racket" "flp" 28 90 (1 2 3)))
;;(filter-in null? '(a 2 () (1 3) b (()) 7))
;;(filter-in list? '(a (b c) 17 ("Restaurante" "Central"))


;;Punto 5
;;List-index es una función que recibe un predicado y una lista, esta función devuelve
;; Devuelve el índice del primer elemento en la lista que satisface el predicado.
;; <list-index> ::= (list-index <predicate> <list>)
;; <predicate> ::= función_unaria
;; <list> ::= '() | (cons <element> <list>)
;; <element> ::= número | símbolo | string | booleano | <list>
;; <cases> ::= (cond 
;;                [(null? <list>) #f] 
;;                [(<predicate> (car <list>)) <acc>]
;;                [else (list-index <predicate> (cdr <list>) (+ <acc> 1))])

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

; 6. swapper ;
; <swapper> ::= (define (swapper <E1> <E2> <L>) <cases>) ;
; <cases> ::= (cond ;
;                [(eq? <L> '()) '()] ;
;                [(eq? (car <L>) <E1>) (cons <E2> (swapper <E1> <E2> (cdr <L>)))] ;
;                [(eq? (car <L>) <E2>) (cons <E1> (swapper <E1> <E2> (cdr <L>)))] :
;                [else (cons (car <L>) (swapper <E1> <E2> (cdr <L>)))]) ;
; <E1> ::= <any> ;
; <E2> ::= <any> ;
; <L> ::= '() | (cons <any> <L>) ;

(define swapper
  (lambda (E1 E2 L)
    (cond
    ; Primer caso, sucede cuando le pasamos una lista vacía, entonces ;
    ; devolvemos una lista vacía similar a L ;
    [(eq? L '()) '()]
    ; Aqui se mira si el primer elemento de L coincide con E1, si ;
    ; coincide entonces se coloca E2 en la primera posición y se ;
    ; llama recursivamente a swapper con los parametros siendo el ;
    ; nuevo primer elemento de la lista E2 y el resto de la lista L ;
    [(eq? (car L) E1) (cons E2 (swapper E1 E2 (cdr L)))]
    ; Sucede de igual manera que el caso anterior, solo que en este ;
    ; caso se compara con E2 ;
    [(eq? (car L) E2) (cons E1 (swapper E1 E2 (cdr L)))]
    ; Por último, se mira el caso en que no coincida ni E1 ni E2, ;
    ; se coloca el primer elemento de L en la lista y se llama ;
    ; recursivamente a swapper con los parametros y el resto de la ;
    ; lista L ;
    [else (cons(car L) (swapper E1 E2(cdr L)))]
    )
    )
  )

; Casos de Prueba ;
; (swapper 'a 'c '(a a c c a a c c)) ;
; (swapper '1 'c '(c d q e r 1 c)) ;
; (swapper 'm 'n '(q e r t y u)) ;
; (swapper '? '- '(? e - ? - $)) ;

;* PUNTO 7
;; cartesian-product: Lista x Lista -> Lista
;; Uso: (cartesian-product L1 L2) = Lista de tuplas, resultados
;; del producto cartesiano entre L1 y L2 se utiliza my-append para eliminar parentesis
;; Gramatica
;;<cartesian-product> ::='()
;;                    ::= (append <cartesian-product-helper> <cartesian-product>)
;;<cartesian-product-helper> ::= '() 
;                           ::= ((list <e-elemento> <elemento>) <cartesian-product-helper>)
(define cartesian-product
  (lambda (L1 L2)
    ;cartesian-product-helper: Elemento x Lista -> Lista
    ;usage: (cartesian-product-helper e L2) = Tuplas entre un elemento e con cada elemento de L2
    (define cartesian-product-helper
      (lambda (e L2)
        (if (null? L2)
            empty
            (cons
             (list e (car L2))
             (cartesian-product-helper e (cdr L2))
             )
            )
        ))

    (if (or (null? L1) (null? L2))
        empty
        (my-append
         (cartesian-product-helper (car L1) L2)
         (cartesian-product (cdr L1) L2)
         )
        )
    )
  )

;;! Casos de prueba
;;(cartesian-product '(a b c) '(x y))
;;(cartesian-product '(p q r) '(5 6 7))
;;(cartesian-product '(p "HOLA" r) '(5 "MUNDO" 7))
;;(cartesian-product '(p (2 3) 4) '(5 (6 7) 7))


;;Punto 8
;; mapping: (X -> Y) Lista Lista -> Lista
;; Recibe una función `F`, dos listas `L1` y `L2`, y devuelve una lista de pares
;; donde `F` aplicado a un elemento de `L1` es equivalente al elemento correspondiente de `L2`.
;; Filtra pares (x, y) donde F(x) == y en listas paralelas.
;; <mapping> ::= (mapping <function> <list1> <list2>)
;; <function> ::= función_unaria
;; <list1> ::= '() | (cons <element1> <list1>)
;; <list2> ::= '() | (cons <element2> <list2>)
;; <element1> ::= número | símbolo | string | booleano
;; <element2> ::= número | símbolo | string | booleano
;; <cases> ::= (cond 
;;                [(or (null? <list1>) (null? <list2>)) '()] 
;;                [(eqv? (<function> (car <list1>)) (car <list2>))
;;                    (cons (list (car <list1>) (car <list2>)) 
;;                          (mapping <function> (cdr <list1>) (cdr <list2>)))]
;;                [else (mapping <function> (cdr <list1>) (cdr <list2>))])

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

; 9. inversions ;
; <inversions> ::= (define (inversions <L>) <cases>) ;
; <cases> ::= (cond ;
;                [(eq? <L> '()) 0] ; 
;                [else (+ (counter (car <L>) (cdr <L>)) (inversions (cdr <L>)))]) ;

; <counter> ::= (define (counter <x> <Ls>) <counter_cases>) ;
; <counter_cases> ::= (cond ;
;                        [(eq? <Ls> '()) 0] 
;                        [(> <x> (car <Ls>)) (+ 1 (counter <x> (cdr <Ls>)))] 
;                        [else (counter <x> (cdr <Ls>))])
; <L> ::= '() | (cons <number> <L>)
; <x> ::= <number>
; <Ls> ::= '() | (cons <number> <Ls>)

(define (inversions L)
  ; Definimos una función adicional counter para las comparaciones ;
  ; formales de las componentes de la lista, donde i < j y a_i > a_j ;
  (define counter
    ; Pasamos 2 parametros, x "primer elemento de la lista" y Ls ;
    ; "será el resto de la lista y valores con los que compararemos ;
    (lambda (x Ls)
    (cond
      ; En caso de que Ls sea una lista vacia, devuelve 0 ;
      [(eq? Ls '()) 0]
      ; En caso de que no, y que nuestro x sea mayor que la siguiente ;
      ; componente, sumamos uno a la funcion y la volvemos a llamar ;
      ; recursivamente para comparar el resto de la lista ;
      [(> x (car Ls)) (+ 1 (counter x (cdr Ls)))]
      ; en caso de que no suceda que x > (car Ls) sencillamente ;
      ; llamamos de forma recursiva con nuestro x y el resto de la lista;
      [else (counter x (cdr Ls))]
      )
    )
    )
  (cond
    ; Vemos si la lista L es vacia, para antes de entrar a counter ;
    ; devolver 0 como valor total de inversión ;
    [(eq? L '()) 0]
    ; en este caso sumamos el llamado de counter con el primer ;
    ; elemento de L y el resto de la lista con el llamado a ;
    ; inversiones con el resto de la lista L "para caer en el caso ;
    ; de parada una vez la lista no tenga más elementos ;
    [else (+ (counter (car L) (cdr L)) (inversions (cdr L)))]
    )
  )

; Casos de Prueba ;
; (inversions '(2 3 8 6 1)) ;
; (inversions '(4 3 2 1)) ;
; (inversions '(10 50 70 200)) ;

;* PUNTO 10
;; up: Lista -> Lista
;; Usage: (up L) = Lista similar a L pero se remueve un nivel de paréntesis (si lo tiene)
;; a cada elemento del nivel más alto de la lista usando my-append del punto 4.
;; Gramatica
;; <up> ::= '()
;;      ::= (<elemento> <up>)
;;
(define up
  (lambda (L)
    (if (null? L)
        empty
        (if (list? (car L)) (my-append (car L)(up (cdr L)))
            (cons (car L) (up (cdr L)))
            )
        )
    )
  )

;;! Casos de prueba
;;(up '((1 2) (3 4)))
;;(up '((x (y)) z))
;;(up '((1 2 3)4)
;;(up '(("ES" "UNA" "LISTA")4 (K ("LISTA" "BAJA"))))


;;Punto 11
;; Recibe una función F y dos listas L1 y L2, y devuelve una nueva lista 
;; aplicando F a cada par de elementos correspondientes de L1 y L2
;; Aplica F a cada par de elementos de L1 y L2 para crear una nueva lista.
;; <zip> ::= (zip <binary-function> <list1> <list2>)
;; <binary-function> ::= función_binaria
;; <list1> ::= '() | (cons <element1> <list1>)
;; <list2> ::= '() | (cons <element2> <list2>)
;; <element1> ::= número | símbolo | string | booleano
;; <element2> ::= número | símbolo | string | booleano
;; <cases> ::= (cond 
;;                [(or (null? <list1>) (null? <list2>)) '()] 
;;                [else (cons (<binary-function> (car <list1>) (car <list2>)) 
;;                           (zip <binary-function> (cdr <list1>) (cdr <list2>)))])

(define zip
  (lambda (F L1 L2)
    (if (or (null? L1) (null? L2))  ;; Si alguna de las listas está vacía no continua el código 
        '()
        (cons (F (car L1) (car L2))  ;;Aplicamos el else, donde se aplica F al primer par de elementos
              (zip F (cdr L1) (cdr L2))))))  ;; Llamada recursiva con el resto de los elementos


;;Casos de prueba
;;(zip + '(1 4) '(6 2))
;;(zip * '(11 5 6) '(10 9 8))

; 12. filter-acum ;
; <filter-acum> ::= (define (filter-acum <a> <b> <F> <acum> <filter>) <cases>) ;
; <cases> ::= (cond ;
;                [(> <a> <b>) <acum>] ; 
;                [(<filter> <a>) (filter-acum (+ <a> 1) <b> <F> (<F> <acum> <a>) <filter>)] ;
;                [else (filter-acum (+ <a> 1) <b> <F> <acum> <filter>)] ) ;
; <a> ::= <number> ;
; <b> ::= <number> ;
; <F> ::= (lambda (<acum> <a>) <expression>) ;
; <acum> ::= <value> ;
; <filter> ::= (lambda (<a>) <boolean>) ;
; <value> ::= <number> | <expression> ;
; <boolean> ::= #t | #f ;

(define filter-acum
  (lambda (a b F acum filter)
    (cond
      ; Como caso base, tenemos que si a > b la función nos devolverá ;
      ; el acumulado ;
      [(> a b) acum]
      ; En caso de que a cumpla con el filtro "este retornará #t si lo ;
      ; cumple o #f en caso de que no", si lo cumple se aplicará la ;
      ; función F con el acum y a ;
      [(filter a) (filter-acum (+ a 1) b F (F acum a) filter)]
      ; En caso de que, sencillamente llama de forma recursiva sin ;
      ; aplicar nada y el acum se mantiene igual ;
      [else (filter-acum (+ a 1) b F acum filter)]
      )
    )
  )

; Casos de Prueba ;
; (filter-acum 1 10 + 0 odd?) ;
; (filter-acum 1 10 + 0 even?) ;
; (filter-acum 5 50 + 0 odd?) ;
; (filter-acum 0 2 + 0 even?) ;

;* PUNTO 13
;; operate: Lista[Función Binaria] x Lista[Enteros] -> Entero
;; Usage: (operate lrators lrands) = Resultado de aplicar sucesivamente las operaciones
;; de lrators a los valores de lrands
;; Gramatica:
;; <operate> ::= (<entero>)
;;           ::= (<lista-operadores> <lista-enteros>)
(define operate
  (lambda (lrators lrands)
    (if (equal? (length lrands) 1)
        (car lrands)
        (operate
         (cdr lrators)
         (cons
          ((car lrators) (car lrands) (cadr lrands))
          (cdr (cdr lrands))
          ))
        )
    ))

;;! Casos de prueba
;;(operate (list + * + - *) '(1 2 8 4 11 6))
;;(operate (list *) '(4 5))
;;(operate (list * * * +) '(2 2 2 2 1))
;;(operate (list * - / +) '(1 2 5 2 10))

;;Punto 14
;; Recibe un número n y un Árbol Binario de Búsqueda (`BST`).
;; Retorna una lista de símbolos left o right que representan el camino
;; para encontrar n en el árbol.
;; Encuentra la ruta (left o right) para ubicar un número en un BST.
;; <path> ::= (path <number> <BST>)
;; <BST> ::= '() | (cons <number> (cons <left-subtree> (cons <right-subtree> '())))
;; <left-subtree> ::= <BST>
;; <right-subtree> ::= <BST>
;; <number> ::= número
;; <cases> ::= (cond 
;;                [(null? <BST>) '()] 
;;                [(= <number> (car <BST>)) '()]
;;                [(< <number> (car <BST>)) 
;;                    (cons 'left (path <number> (cadr <BST>)))]
;;                [(> <number> (car <BST>)) 
;;                    (cons 'right (path <number> (caddr <BST>)))])

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

; 15. count-odd-and-even arbol ;
; <count-odd-and-even> ::= (define (count-odd-and-even <arbol>) <cases>)
; <cases> ::= (cond ;
;                [(eq? <arbol> '()) '(0 0)] ; 
;                [else (<counter> (car <arbol>) ;
;                                 (count-odd-and-even (cadr <arbol>)) ;
;                                 (count-odd-and-even (caddr <arbol>)))] ) ;
; <counter> ::= (lambda (<val> <izq> <der>) ;
;                  (list (+ (<even-check> <val>) (car <izq>) (car <der>)) ;
;                        (+ (<odd-check> <val>) (cadr <izq>) (cadr <der>)))) ; ;
; <even-check> ::= (lambda (<val>) (cond [(even? <val>) 1] [else 0])) ;
; <odd-check> ::= (lambda (<val>) (cond [(odd? <val>) 1] [else 0])) ;
; <arbol> ::= '() | (<val> <subarbol> <subarbol>) ;
; <subarbol> ::= <arbol> ;
; <val> ::= <number> ;



(define count-odd-and-even
  (lambda (arbol)
    ; La función counter recibirá un valor val y 2 listas con los ;
    ; conteos correspondientes a los subarboles izq y der, ;
    ; de modo que al final este nos devolverá una lista con el total ;
    ; de numeros pares e impares de nuestro arbol ;
    (define counter
    (lambda (val izq der)
      (list (+ (cond
                 [(even? val) 1] [else 0]) (car izq) (car der))
            (+ (cond
                 [(odd? val) 1] [else 0]) (cadr izq) (cadr der))
            )
    )
    )
    (cond
      ; Caso base: En caso de que el arbol sea vacio, nos devolverá ;
      ; una lista (0 0) ya que no habrá ni pares ni impares ;
      [(eq? arbol '()) '(0 0)]
      ; En caso de que no ocurra el caso base, este hace 2 llamados ;
      ; recursivos a count-odd-and-even para los subarboles izq y der ;
      ; y se usa counter para obtener los resultados ;
      [else (counter (car arbol)
                     (count-odd-and-even (cadr arbol))
                     (count-odd-and-even (caddr arbol)))]
      )
    )
  )

; Casos de Prueba ;
(count-odd-and-even '(14 (7 () (12 () ()))
                           (26 (20 (17 () ())
                                   ())
                               (31 () ()))))
(count-odd-and-even '(20 (1 () (13 () ()))
                           (41 (15 (42 () ())
                                   ())
                               (12 () ()))))
; No los comentaremos para terminos practicos :D ;



;;Punto 17
;; Recibe una matriz mat y un vector vec que devuelve una nueva matriz
;; donde cada elemento de una fila de mat ha sido multiplicado escalarmente
;; por el elemento correspondiente del vec.
;; Multiplica cada fila de la matriz por el vector dado.
;; <prod-scalar-matriz> ::= (prod-scalar-matriz <matrix> <vector>)
;; <matrix> ::= '() | (cons <row> <matrix>)
;; <row> ::= '() | (cons <number> <row>)
;; <vector> ::= '() | (cons <number> <vector>)
;; <number> ::= número
;; <cases> ::= (cond 
;;                [(null? <matrix>) '()] 
;;                [else (cons (map * (car <matrix>) <vector>) 
;;                           (prod-scalar-matriz (cdr <matrix>) <vector>))])

(define prod-scalar-matriz
  (lambda (mat vec)
    (if (null? mat)  ;; Si la matriz está vacía, retornamos una lista vacía
        '()
        (cons (map * (car mat) vec)  ;; Multiplicamos fila por fila con el vector
              (prod-scalar-matriz (cdr mat) vec)))))  ;; Recurrimos con la siguiente fila

;; Casos de Prueba
;;(prod-scalar-matriz '((1 1) (2 2)) '(2 3))
;;(prod-scalar-matriz '((1 1) (2 2) (3 3)) '(2 3))
;;(prod-scalar-matriz '((1 2) (3 4)) '(2 3))  
;;(prod-scalar-matriz '((1 0 4) (2 5 3)) '(2 3 1))  
;;(prod-scalar-matriz '((10 20 30) (40 50 60)) '(1 2 3))  

; 18. pascal N ;
; <pascal> ::= (define (pascal <n>) <cases>) ;
; <cases> ::= (cond ; 
;                [(= <n> 1) '(1)] ;
;                [else (cons 1 (<colum> (pascal (- <n> 1))))] ) ;
; <colum> ::= (lambda (<col>) ;
;                (cond [(eq? (cdr <col>) '()) '(1)]
;                      [else (cons (+ (car <col>) (cadr <col>)) ;
;                                  (<colum> (cdr <col>)))]) ) ;
; <n> ::= <positive-integer> ;
; <col> ::= <list-of-numbers> ;


(define pascal
  (lambda (n)
  (define colum
    ; Creamos una función auxiliar que nos ayudara a generar la nueva ;
    ; fila sumando los elementos como en el triángulo de pascal ;
    (lambda (col)
      (cond
        ; Caso base, donde se agrega un 1 al final de la lista  cuando ;
        ; ya no quedan elementos restante en la lista ;
        [(eq? (cdr col) '()) '(1)]
        ; Aqui añadimos la suma de los elementos adyacente "1 + 1" "2" ;
        ; y creamos una lista con los resultados ;
        [else (cons (+ (car col) (cadr col))
                    ; Por último, llamamos recursivamente lo que queda ;
                    ; de la fila ;
                    (colum (cdr col)))]
        )
      )
    )
    (cond
      ; Este es el caso base de Pascal, donde si n es 1, nos retorna (1) ;
      [(= n 1) '(1)]
      ; aqui añadimos 1 al resultante de invocar la función auxiliar ;
      ; colum que en base a la fila anterior de pascal calculara la nueva ;
      ; fila ;
      [else (cons 1 (colum (pascal (- n 1))))]
      )
  )
  )

; Casos de Prueba ;
; (pascal 1) ;
; (pascal 5) ;
; (pascal 7) ;