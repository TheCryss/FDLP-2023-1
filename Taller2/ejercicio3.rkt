#lang eopl
(require "ejercicio1.rkt")

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

(define list1 (list 1 2))
(define list2(list 3 4))

;; aux :
;; Proósito: Number List -> List
;; Procedimiento que crea una tupla entre 'x' y cada elemento de la lista 'L',
;; retornando una lista de tuplas.
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
(define t (generator  '(1 2 3) '(#t #f)))


;; element-n :
;; Proposito: L' -> n
;; Procedimiento que dada una lista l, obtiene el elmento n de dicha lista.
;; Teniendo en cuenta que tomaremos que una lista esta indexada desde 1.
;; <lista> := ()
;;         := (<valor-de-scheme> <lista>)
(define element-n
  (lambda (l n)
    [cond
      [(eqv? n 1)
        (car l)
      ]
      [else
        (element-n (cdr l) (- n 1))
      ]
    ]
  )
)

;; eval-clause :
;; Proposito: clause-l comb-l -> bool
;; Procedimiento que dado una clausula (lista de numeros + o -) y una combinacion
;; de valores #t y #f, evalua la clausula.
;; <or> ::= OR (<var> {<var>}*)
(define eval-clause
  (lambda (clause comb)
    [cond
      [(null? (cdr clause))
        (if (positive? (car clause))
          (element-n comb (car clause)) 
          (not (element-n comb (abs (car clause))))
        )
      ]
      [else
        (or (eval-clause (list (car clause)) comb) (eval-clause (cdr clause) comb))
      ]
    ]
  )
)

;; eval-clauses :
;; Proposito: clauses-l comb-l -> bool
;; Procedimiento que dado una lista de clausulas (listas de numeros + o -) y una 
;; combinacion de valores #t y #f, evalua las clausulas.
;; <and> ::= AND (<or> {<or>}*) 
;; <or> ::= OR (<var> {<var>}*)
(define eval-clauses
  (lambda (clauses comb)
    [cond
      [(null? (cdr clauses))
        (eval-clause (or-list->varlist (car clauses)) comb)
      ]
      [else
        (and
          (eval-clause (or-list->varlist (car clauses)) comb) 
          (eval-clauses (cdr clauses) comb)
        )
      ]
    ]
  )
)

;; range :
;; Proposito: acum n -> L'
;; Procedimiento que dado un acumulador y un numero n, genera una lista de 
;; numeros desde acumulador hasta n
;; <lista> := ()
;;         := (<valor-de-scheme> <lista>)
(define range
  (lambda (acum n)
    [cond
      [(eqv? n acum)
        (list n)
      ]
      [else
        (append (list acum) (range (+ acum 1) n))
      ]
    ]
  )
)

;; eval-fnc :
;; Proposito: clauses-l combs-l -> L'
;; Procedimiento que dado una lista de clausulas (listas de numeros + o -) y una lista
;; de combinaciones de valores #t y #f, evalua cada las clausulas con cada
;; combinacion hasta encontrar una solucion e indicarla o no encontrarla.
;; <and> ::= AND (<or> {<or>}*) 
;; <or> ::= OR (<var> {<var>}*)
(define eval-fnc
  (lambda (clauses combinaciones)
    [cond
      [(null? combinaciones)
        (list 'insatisfactible combinaciones)
      ]
      [(eval-clauses clauses (car combinaciones))
        (list 'satisfactible (car combinaciones))
      ]
      [else
        (eval-fnc clauses (cdr combinaciones))
      ]
    ]
  )
)

;; EVALUARSAT :
;; Proposito: fnc -> L'
;; Procedimiento que dado una instancia SAT (fnc), evalua si tiene solucion.
;; <fnc> ::= FNC num-vars (<and>)
;; <and> ::= AND (<or> {<or>}*) 
;; <or> ::= OR (<var> {<var>}*)
;; <var> ::= 1 | -1 | 2 | -2 | ... | num-vars | -num-vars
(define EVALUARSAT
  (lambda (fnc)
    (letrec
      (
        (and (fnc-list->and fnc))
        (comb-aux (generator  (range 1 (fnc-list->var fnc)) '(#t #f)))
        (combinaciones (mix (car comb-aux) (cdr comb-aux)))
      )
      (eval-fnc (and-list->clausulas and) combinaciones)
    )
  )
)