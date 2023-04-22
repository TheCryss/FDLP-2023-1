#lang eopl
(require "ejercicio1.rkt")

; Estudiantes:
; Jose Luis Hincapie Bucheli - 2125340
; Sebatian Idrobo Avirama - 2122637
; Juan Sebastian Getial Getial - 2124644

;;; 2. Funciones Parse y Unparse

;Función Parse
(define parser-aux
    (lambda (fnc-l)
        [cond
            [(null? fnc-l)
                fnc-l
            ]
            [(eqv? (car fnc-l) 'AND)
                (and-list (parser-aux (cadr fnc-l)))
            ]
            [(eqv? (caar fnc-l) 'OR)
                (cons (or-list (cadar fnc-l)) (parser-aux (cdr fnc-l)))
            ]
        ]
    )
)

(define PARSEBNF
    (lambda (fnc)
        (let
            (
                [fnc-l (caddr fnc)]
                [num-vars (cadr fnc)]
            )
            (fnc-list num-vars (parser-aux fnc-l))
        )
    )
)

;; Ejemplos
(define entrada1 (list 'FNC 2 (list 'AND (list (list 'OR (list 1 2)) (list 'OR (list -1))))))
(define entrada2 (list 'FNC 3 (list 'AND (list 
                                            (list 'OR (list 1 2)) 
                                            (list 'OR (list -1)) 
                                            (list 'OR (list 1 2 -3))))))
(PARSEBNF entrada1)
(PARSEBNF entrada2)

;Función Unparse
;(define UNPARSEBNF
;  (lambda (tree)
;    (cond
;      [])
;    ))
