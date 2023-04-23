#lang eopl
(require "ejercicio1.rkt")

; Estudiantes:
; Jose Luis Hincapie Bucheli - 2125340
; Sebatian Idrobo Avirama - 2122637
; Juan Sebastian Getial Getial - 2124644

;;; 2. Funciones Parse y Unparse

;; PARSEBNF :
;; Proposito: fnc-l -> fnc
;; Procedimiento que dada una lista con la representación
;; concreta de una instancia 'SAT' (fnc), 'and-list' o 'or-list', 
;; construye el árbol de sintáxis abstracta basado en listas.

(define PARSEBNF
  (lambda (fnc)
    [cond
      [(null? fnc)
       fnc
       ]
      [(eqv? (car fnc) 'fnc)
       (fnc-list (cadr fnc) (PARSEBNF (caddr fnc)))
       ]
      [(eqv? (car fnc) 'and)
       (and-list (PARSEBNF (cadr fnc)))
       ]
      [(eqv? (car fnc) 'or)
       (or-list (cadr fnc)) 
       ]  
      [(eqv? (caar fnc) 'or)
       (cons (PARSEBNF (car fnc)) (PARSEBNF (cdr fnc)))
       ]
      ]))


;; Ejemplos
(define entrada1 (list 'fnc 2 (list 'and (list (list 'or (list 1 2)) (list 'or (list -1))))))
(define entrada2 (list 'fnc 3 (list 'and (list 
                                            (list 'or (list 1 2)) 
                                            (list 'or (list -1)) 
                                            (list 'or (list 1 2 -3))))))
(define entrada3 (list 'and (list
                             (list 'or (list 1 2))
                             (list 'or (list -1 3)))))
(define entrada4 (list 'or (list 1 2 3)))

(PARSEBNF entrada1)
(PARSEBNF entrada2)
(PARSEBNF entrada3)
(PARSEBNF entrada4)


;Funcion unparse
;; UNPARSEBNF :
;; Proposito: tree -> fnc-l
;; Procedimiento que dado un árbol de sintáxis abstracta
;; de una instancia 'SAT' (fnc), 'and-list' o 'or-list', entregue 
;; la representación concreta basada en listas.
(define UNPARSEBNF
    (lambda (tree)
        [cond
            [(null? tree) 
                tree
            ]
            [(eqv? (car tree) 'FNC)
                (list 'fnc (fnc-list->var tree) (UNPARSEBNF (fnc-list->and tree)))
            ]
            [(eqv? (car tree) 'AND)
                 (list 'and (UNPARSEBNF (and-list->clausulas tree)))]
            [(eqv? (car tree) 'OR)
                (list 'or (or-list->varlist tree))
            ]
            [(eqv? (caar tree) 'OR)
                (cons (UNPARSEBNF (car tree)) (UNPARSEBNF (cdr tree)))
            ]
        ]        
    )
)

;; Ejemplos
(UNPARSEBNF (PARSEBNF entrada1))
(UNPARSEBNF (PARSEBNF entrada2))
(UNPARSEBNF (PARSEBNF entrada3))
(UNPARSEBNF (PARSEBNF entrada4))
