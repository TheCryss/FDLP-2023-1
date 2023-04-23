#lang eopl

; Estudiantes:
; Jose Luis Hincapie Bucheli - 2125340
; Sebatian Idrobo Avirama - 2122637
; Juan Sebastian Getial Getial - 2124644


;;Creadon los "frascos"

(define cartesian-product
  (lambda (L1 L2)
    (cond
      [(null? L1) '()]
      [else (append (aux (car L1) L2) (cartesian-product (cdr L1) L2))])
    )
  )
 
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