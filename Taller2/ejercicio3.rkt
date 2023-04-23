#lang eopl

; Estudiantes:
; Jose Luis Hincapie Bucheli - 2125340
; Sebatian Idrobo Avirama - 2122637
; Juan Sebastian Getial Getial - 2124644


;;Creadon los "frascos"

;; cartesian-product :
;; Proposito: L1 L2 -> List
;; Procedimiento que dados dos listas 'L1' y 'L2' realiza un producto cartesiano
;; entre las dos listas.
(define cartesian-product
  (lambda (L1 L2)
    (cond
      [(null? L1) '()]
      [else (append (aux (car L1) L2) (cartesian-product (cdr L1) L2))])
    )
  )
<<<<<<< HEAD
 
=======

(define list1 (list 1 2))
(define list2(list 3 4))

;; aux :
;; Proósito: Number List -> List
;; Procedimiento que crea una tupla entre 'x' y cada elemento de la lista 'L',
;; retornando una lista de tuplas.
>>>>>>> 4d0cb4801a90113d5ac2d361699d29a0072fe539
(define aux
  (lambda (x L)
    (cond 
      [(null? L) empty]
      [else (cons ( append x (car L)) (aux x (cdr L)))])
    ) 
  )

; para 3 variables
;(cartesian-product '(a b c) '(#t #f)

;Combinando los "frascos"

(define mix
  (lambda (cabezaLista restoLista)
    (cond
      [(null? restoLista) cabezaLista]
      [else (mix (cartesian-product cabezaLista (car restoLista)) (cdr restoLista) )]
      )
    
    )
  )

(define generator
  (lambda (L1 L2)
    (letrec
        (
         (aux (lambda (x L)
            (cond 
              [(null? L) empty]
              [else (cons (list  (car L)) (aux x (cdr L)))])
            )
          )
         )
      
       (cond
         [(null? L1) '()]
         [else (cons (aux (car L1) L2) (generator (cdr L1) L2))]
       
      )
    )
    )
 )



;pruebas
(define t (generator  '(1 2 3 4 5 6) '(#t #f)))