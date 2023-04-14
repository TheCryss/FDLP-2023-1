#lang eopl

; Estudiantes:
; Jose Luis Hincapie Bucheli - 2125340
; Sebatian Idrobo Avirama - 2122637
; Juan Sebastian Getial Getial - 2124644

; Procedimientos:

;; Ejercicio 01
;; invert :
;; Proposito: L x P -> L
;; Procedimiento que dada una lista de pares aplica un predicado
;; si este se cumple retornara dicho par pero con los valores invertidos
;; <lista> := ()
;;         := (<valor-de-scheme> <lista>)

(define invert
  (lambda (lista predicado)
    (if (null? lista) empty
        ( if(and (predicado (caar lista)) (predicado (cadar lista)))
            (cons (list (cadar lista) (caar lista)) (invert (cdr lista) predicado)) (invert (cdr lista) predicado) )
        )
    )
  )

(define multiplo5?
  (lambda (n)
         (cond
           [(< n 0) #f]
           [(eqv? n 0) #t]
           [else (multiplo5? (- n 5))]
           )
         )
  )

;; Pruebas
(invert '((3 2) (4 2) (1 5) (2 8)) even?)
(invert '((5 9) (10 90) (82 7) ) multiplo5? )
(invert '((6 9) (10 90) (82 7) ) odd? )


;; Ejercicio 02
;; down :
;; Proposito: L -> L
;; Procedimiento que ingresa una lista L y retorna una lista con cada elemento
;; de L asociado a un nivel más de paréntesis comparado con su estado original.
;;
;; <lista> := ()
;;         := (<valor-de-scheme> <lista>)

(define down (
  lambda (L)
   [cond
     [(null? L)
      empty
      ]
     [else
      (cons (cons (car L) empty) (down (cdr L)))]]))

;; Pruebas
(down '(1 2 (3) 4 5))
(down '(hola como (estas (?)) 1))


;; Ejercicio 03
;; list-set :
;; Proposito:
;; L x n x X x P -> L' : Procedimiento que ingresa el elemento X en la
;; posicion ingresada n (indexando desde cero) de la lista L solo si el elemento
;; original de la lista cumple con el predicado P.
;;
;; <lista> := ()
;;         := (<valor-de-scheme> <lista>)

(define list-set (
    lambda (L n X P) 
        [cond
            [(eqv? n 0)
                (if (P (car L))
                    (cons X (cdr L))
                    L
                )
            ]
            [else
                (cons (car L) (list-set (cdr L) (- n 1) X P))
            ]
        ]
))

;; Pruebas
(list-set '(5 8 7 6) 2 '(1 2) odd?)
(list-set '(5 8 7 6) 2 '(1 2) even?)


;; Ejercicio 04
;; filter-in :
;; Proposito: L x P -> L
;; Procedimiento que dada una lista se le aplica un predicado
;; los elementos que lo cumplan seran devueltos en una lista
;; <lista> := ()
;;         := (<valor-de-scheme> <lista>)


(define filter-in
  (lambda (P L)
    (cond
      [(null? L) empty]
      [(P (car L)) (cons (car L) (filter-in P (cdr L)))]
      [else (filter-in P (cdr L))]
      )
    )
  )

;;Pruebas
(filter-in number? '(a 2 (1 3) b 7))
(filter-in symbol? '(a (b c) 17 foo))
(filter-in string? '(a b u "univalle" "racket" "flp" 28 90 (1 2 3)))


;; Ejercicio 05
;; list-index :
;; Propósito: P x L -> Int | #f
;; Procedimiento que retorna la posicion del primer elemento de la lista L tal
;; que satisfaga el predicado P. En caso de que ninguún elemento cumpla, retorna #f
;;
;; <lista> := ()
;;         := (<valor-de-scheme> <lista>)
(define list-index (
  lambda (P L)
    (cond
      [(null? L) #f]
      [(P (car L)) 0]
      [else
        (let ((contador (list-index P (cdr L))))
          (if (number? contador)
              (+ 1 contador)
              #f))])))

;;Pruebas
(list-index number? '(b (1 3) c 3 d))
(list-index list? '(b (1 3) c 3 d))
              

;; Ejercicio 06 
;; swapper :
;; Proposito:
;; E1 x E2 x L -> L' : Procedimiento que reemplaza cada ocurrencia anterior de E1
;; por E2 y cada ocurrencia anterior de E2 por E1 (Los elementos
;; E1 y E2 deben pertenecer a L) de la lista L
;;
;; <lista> := ()
;;         := (<valor-de-scheme> <lista>)

(define swapper
    (lambda (E1 E2 L)
        [cond
            [(null? L) empty]
            [else 
                (cond
                    [(eqv? (car L) E1)
                        (cons E2 (swapper E1 E2 (cdr L)))
                    ]
                    [(eqv? (car L) E2)
                        (cons E1 (swapper E1 E2 (cdr L)))
                    ]
                    [else
                        (cons (car L) (swapper E1 E2 (cdr L)))
                    ]
                )
            ]
        ]
    )
)

;; Pruebas
(swapper 'a 'd '(a b c d))
(swapper 'a 'd '(a d () c d))
(swapper 'x 'y '(y y x y x y x x y))


;; Ejercicio 07
;; cartesian-product :
;; Proposito: L x L -> L
;; Realizar el producto cartesiano entre 2 listas
;; <lista> := ()
;;         := (<valor-de-scheme> <lista>)


(define cartesian-product
  (lambda (L1 L2)
    (cond
      [(null? L1) '()]
      [else (append (aux (car L1) L2) (cartesian-product (cdr L1) L2))])
    )
  )


;; aux :
;; Proposito: Int x L -> L
;; Realizar el producto cartesiano entre una lista y un entero
;; <lista> := ()
;;         := (<valor-de-scheme> <lista>)

(define aux
  (lambda (x L)
    (cond 
      [(null? L) empty]
      [else (cons (list x (car L)) (aux x (cdr L)))])
    )
  )

;; Pruebas
(cartesian-product '(a b c) '(x y))
(cartesian-product '(p q r) '(5 6 7))


;; Ejercicio 08
;; mapping:
;; Propósito: F x L1 x L2 -> L
;; Procedimiento que retorna una lista de pares (a b) tales que F(a) = B en base a la
;; función F, siendo 'a' elemento de L1 y 'b' elemento de L2
;;
;; <lista> := ()
;;         := (<valor-de-scheme> <lista>)

(define mapping
  (lambda (F L1 L2)
    [cond
      [(not (equal? (length L1) (length L2))) (eopl:error '"Lists have different length")]
      [(null? L1) empty]
      [(equal? (F (car L1)) (car L2)) (cons (list (car L1) (car L2)) (mapping F (cdr L1) (cdr L2)))]
      [else (mapping F (cdr L1) (cdr L2))]]))

;; Pruebas
(mapping (lambda (x) (* x 4)) (list 1 2 3) (list 4 8 12))
(mapping (lambda (x) (+ x 2)) (list 2 3 4) (list 4 0 6))


;; Ejercicio 09
;; inversions :
;; Proposito:
;; L -> n : Procedimiento que determina el numero de inversiones de la lista L. 
;; De manera formal, sea A = (a1 a2...an) una lista de x numeros diferentes, 
;; si i < j (posicion) y ai > aj (dato en la posicion) entonces la pareja (i j) 
;; es una inversion de A
;;
;; <lista> := ()
;;         := (<valor-de-scheme> <lista>)

    ;; inversions_aux :
    ;; Proposito:
    ;; L x acc -> n : Procedimiento que determina el numero de inversiones del 
    ;; primer elemento con los demas elementos de la lista L. 
    ;; De manera formal, sea A = (a1 a2...an) una lista de x numeros diferentes, 
    ;; si j > 1 (posicion) y a1 > aj (dato en la posicion) entonces la pareja (a1 aj) 
    ;; es una inversion de la lista L con base al elemento a1
    ;;
    ;; <lista> := ()
    ;;         := (<valor-de-scheme> <lista>)

    (define inversions_aux
        (lambda (L acc)
            [cond
                [(or (null? L) (null? (cdr L)))
                    acc
                ]
                [(> (car L) (cadr L))
                    (inversions_aux (cons (car L) (cddr L)) (+ acc 1))
                ]
                [else
                    (inversions_aux (cons (car L) (cddr L)) acc)
                ]
            ]
        )
    )

    ;; Pruebas
    (inversions_aux '(2 3 8 6 1) 0)
    (inversions_aux '(1 2 3 4) 0)
    (inversions_aux '(3 2 1) 0)

(define inversions
    (lambda (L)
        [cond
            [(or (null? L) (null? (cdr L)))
                0
            ]
            [
                (+ (inversions_aux L 0) (inversions (cdr L)))
            ]
        ]
    )
)

;; Pruebas
(inversions '(2 3 8 6 1))
(inversions '(1 2 3 4))
(inversions '(3 2 1))


;; Ejercicio 10
;; up :
;; Proposito: L -> L
;; Recibe una Lista y sube un nivel todos los elementos dentro de ella
;; en caso de estar en el nivel mas alto lo deja ahi.
;; <lista> := ()
;;         := (<valor-de-scheme> <lista>)


(define up (
  lambda (L)
   [cond
     [(null? L)
      empty
      ]
     [(list?(car L)) (append (append (car L) empty) (up (cdr L)))]
     [else
       (list (car L))]]))

;; Pruebas
(up '((1 2) (3 4)))
(up '((x (y)) z))


;; Ejercicio 11
;; zip
;; Propósito: F x L1 x L2 -> L
;; Procedimiento que retorna una lista tal que la posición n-ésima es resultado
;; de aplicar la función F sobre los elementos L1 y L2 en las respectivas posiciones.
(define zip
  (lambda (F L1 L2)
    [cond
      [(not (equal? (length L1) (length L2))) (eopl:error '"Lists have different length")]
      [(null? L1) empty]
      [else (cons (F (car L1) (car L2)) (zip F (cdr L1) (cdr L2)))]]))

;; Pruebas
(zip - '(5 4 0) '(1 4 2)) ;(4 0 -2)
(zip * '(5 4 0) '(1 4 2)) ;(5 16 0)


;; Ejercicio 12
;; filter-acum :
;; Proposito :
;; a x b x F x acum x filter -> n : Procedimiento que aplica la 
;; función binaria F a todos los elementos que están en el intervalo 
;; [a, b] y que a su vez todos estos elementos cumplen con el 
;; predicado de la función filter, el resultado se va conservando 
;; en acum y finalmente se retorna el valor final de acum.
;;
;; <lista> := ()
;;         := (<valor-de-scheme> <lista>)

(define filter-acum
    (lambda (a b F acum filter)
        [cond
            [(> a b)
                acum
            ]
            [(filter a)
                (filter-acum (+ a 1) b F (F acum a) filter)
            ]
            [else
                (filter-acum (+ a 1) b F acum filter)
            ]
        ]
    )
)

;; Pruebas
(filter-acum 1 10 + 0 odd?)
(filter-acum 1 10 + 0 even?)


;; Ejercicio 13
;; operate
;; Propósito: L x L -> L
;; Procedimiento que aplica una serie de funciones binarias a una lista
;; de valores dados.
;; <lista> := ()
;;         := (<valor-de-scheme> <lista>)

(define operate
  (lambda (Irators Irands)
    (cond
      [(null? Irators) car Irands]
      [else ( operate (cdr Irators) (cons ( (car Irators) (car Irands) (cadr Irands)) (cddr Irands))  )
       ]
     )
   )
  )

;; Pruebas
 (operate (list + * + - *) '(1 2 8 4 11 6))
 (operate (list *) '(4 5))


;; Ejercicio 14
;; path
;; Propósito: Int x <árbol-binario> -> List
;; Procedimiento que retorna una lista indicando la ruta a tomar en el árbol binario
;; para encontrar el número 'n'.
;;
;; <árbol-binario> := (árbol-vacío) empty
;;                 := (nodo) número <árbol binario> <árbol binario>

(define path
  (lambda (n tree)
    [cond
      [(equal? (car tree) n) '()]
      [(< n (car tree)) (cons 'left (path n (cadr tree)))]
      [(> n (car tree)) (cons 'right (path n (caddr tree)))]]))

;; Pruebas
(path 5 '(2 (1 (0 () ()) ()) (4 () (6 (5 () ()) (8 () ())))))
(path 1 '(2 (1 (0 () ()) ()) (4 () (6 (5 () ()) (8 () ())))))
(path 2 '(2 (1 (0 () ()) ()) (4 () (6 (5 () ()) (8 () ())))))


;; Ejercicio 15
;; count-odd-and-even :
;; Proposito :
;; BST -> (even odd) : Procedimiento que toma un árbol binario y retorna una
;; lista con dos elementos correspondientes a la cantidad de pares e impares 
;; en el arbol.
;;
;; <árbol-binario> := (árbol-vacı́o) empty
;;                 := (nodo) número <árbol-binario> <árbol-binario>

(define count-odd-and-even
    (lambda (BST)
        [cond
            [(null? BST)
                '(0 0)
            ]
            [(not (pair? BST)) ; Si BST no es una lista
                (if (even? BST)
                    '(1 0)
                    '(0 1)
                )
            ]
            [else
              (zip + (count-odd-and-even (car BST)) (count-odd-and-even (cdr BST)))
            ]
        ]
    )
)

;; Pruebas
(count-odd-and-even '(14 (7 () (12 () ()))
                      (26 (20 (17 () ())
                              ())
                          (31 () ()))))
(count-odd-and-even '(1 (4 () (1 () ())) (5 () ())))


;; Ejercicio 16
;; simpson-rule
;; Propósito: f x a x b x n -> z
;; Procedimiento que cálcula la integral de una función f entre los valores
;; a y b mediante la regla de Simpson.
;;
;; <lista> := ()
;;         := (<valor-de-scheme> <lista>)

(define simpson-rule
  (lambda (f a b n)
    (letrec
      (
        (h ( / (- b a) n))
        (y0 (f a))
        (yn (f (+ a (* n h))))
        (aux
          (lambda (f h n)
            [cond
              [(eqv? n 0)
                0
              ]
              [(odd? n)
                (+ 
                  (* 4 (f (+ a (* n h))))
                  (aux f h (- n 1))
                )
              ]
              [else
                (+ 
                  (* 2 (f (+ a (* n h))))
                  (aux f h (- n 1))
                )
              ]
            ]
          )
        )
      )
      (* 
        (/ h 3 )
        (+
          y0
          yn
          (aux f h (- n 1))
        )
      )      
    )
  )
)

;; Pruebas
(simpson-rule (lambda (x) (* x (* x x))) 1 5 8)
(simpson-rule (lambda (x) x) 1 5 12)


;; Ejercicio 17
;; prod-listas (Función Auxiliar)
;; Propósito: L x L -> L
;; Procedimiento que retorna la multiplicación en una lista del primer elemento de L1
;; con el primer elemento de L2 y así sucesivamente
;;
;; <lista> := ()
;;         := (<valor-de-scheme> <lista>)

(define prod-listas
  (lambda (L1 L2)
    [cond
      [(not (equal? (length L1) (length L2))) (eopl:error '"Lists have different length")]
      [(null? L1) empty]
      [else (cons (* (car L1) (car L2)) (prod-listas (cdr L1) (cdr L2)))]]))

;; Pruebas
(prod-listas '(3 5 7) '(1 2 1))
(prod-listas '(1 2 3 4) '(4 3 2 1))

;; prod-scalar-matriz
;; Propósito: L x L -> L:
;; Procedimiento que retorna el resultado de realizar un producto escalar entre la
;; matriz 'mat' y el vector 'vec'.
;; Funciones auxiliares: prod-listas
;;
;; <lista> := ()
;;         := (<valor-de-scheme> <lista>)

(define prod-scalar-matriz
  (lambda (mat vec)
    [cond
      [(null? mat) empty]
      [else (cons (prod-listas (car mat) vec) (prod-scalar-matriz (cdr mat) vec))]
      ]))

;; Pruebas
(prod-scalar-matriz '((1 1) (3 2) (10 5)) '(-2 3))
(prod-scalar-matriz '((1 1 1) (3 2 1) (10 5 0)) '(1 2 3))


;; Ejercicio 18
;; pascal :
;; Proposito :
;; N -> L : Procedimiento que retorna la fila N
;; del triangulo de Pascal.
;;
;; <lista> := ()
;;         := (<valor-de-scheme> <lista>)

  ;; pascal_aux :
  ;; Proposito :
  ;; N x L -> L' : Procedimiento que retorna la fila N
  ;; del triangulo de Pascal recibiendo como prametro
  ;; incial la fila 1.
  ;;
  ;; <lista> := ()
  ;;         := (<valor-de-scheme> <lista>)

  (define pascal_aux
    (lambda (N L) ; L = '(1) Fila 1 del triangulo de pascal
      [cond
        [(eqv? N 1)
          L
        ]
        [else
          (pascal_aux (- N 1) (zip + (cons 0 L) (append L '(0))))
        ]
      ]
    )
  )

  ;; Pruebas
  (pascal_aux 1 '(1))
  (pascal_aux 5 '(1))

(define pascal
  (lambda (N)
    (pascal_aux N '(1))
  )
)

;; Pruebas
(pascal 5)
(pascal 1)