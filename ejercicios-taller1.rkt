#lang eopl

; Estudiantes:
; Jose Luis Hincapie Bucheli - 2125340
; Sebatian Idrobo Avirama - 2122637
; Juan Sebastian Getial Getial - 2124644

; Procedimientos:

;; invert :
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


;; list-index1 :
;; Propósito: P x L -> Int | #f
;; Procedimiento que retorna la posicion del primer elemento de la lista L tal
;; que satisfaga el predicado P. En caso de que ninguún elemento cumpla, retorna #f
;;
;; <lista> := ()
;;         := (<valor-de-scheme> <lista>)

(define list-index1 (
  lambda (P L)
    (cond
      [(null? L) #f]
      [(P (car L)) 0]
      [else
        (let ((contador (list-index1 P (cdr L))))
          (if (number? contador)
              (+ 1 contador)
              #f))])))


;; list-index2 :
;; Propósito: P x L -> Any
;; Procedimiento que retorna el primer elemento de la lista L tal que satisfaga
;; el predicado P. En caso de que ninguún elemento cumpla, retorna #f
;;
;; <lista> := ()
;;         := (<valor-de-scheme> <lista>)
(define list-index2 (
  lambda (P L)
   [cond
     [(null? L) #f]
     [else
      (if (P (car L)) (car L) (list-index2 P (cdr L)))]]))




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


;; filter-acum :
;; Proposito:
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