#lang eopl
; Taller 1 - FLP;

; 3. list-set ;
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

; 6. swapper ;
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

; 9. inversions ;
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

; 12. filter-acum ;
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

; 15. count-odd-and-even arbol ;




