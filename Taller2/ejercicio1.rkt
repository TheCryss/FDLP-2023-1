#lang eopl
(provide or-list and-list fnc-list
         fnc-list->var fnc-list->clausulas or-list->varlist
         SAT-list1 SAT-list2 SAT-list3
         or and sat or-vars and-clauses fnc-exp
         or? and? sat?)

; Estudiantes:
; Jose Luis Hincapie Bucheli - 2125340
; Sebatian Idrobo Avirama - 2122637
; Juan Sebastian Getial Getial - 2124644


;;; 1 Instancias SAT

;; 1.1 Gramatica BNF

; Gramatica FNC
; <fnc> ::= FNC num-vars (<and>)
; <and> ::= AND (<or> {<or>}*) 
; <or> ::= OR (<var> {<var>}*)
; <var> ::= 1 | -1 | 2 | -2 | ... | number-vars | -number-vars

;; Gramatica con listas

; CONSTRUCTORES

;; or-list :
;; Proposito: L -> L'
;; Procedimiento que dada una lista de variables(numeros), genera la sintaxis abstracta
;; de un or basado en listas
;; <or> ::= OR (<var> {<var>}*)
(define or-list
    (lambda (vars-list)
        (list 'OR vars-list)
    )
)

;; and-list :
;; Proposito: L -> L'
;; Procedimiento que dada una lista de or, genera la sintaxis abstracta
;; de un and basado en listas
;; <and> ::= AND (<or> {<or>}*) 
(define and-list
    (lambda (ors-list)
        (list 'AND ors-list)
    )
)

;; fnc-list :
;; Proposito: n L -> L'
;; Procedimiento que dado un n numero de variables y y un and, devuelve
;; la sintaxis abstracta de un fnc bassado en listas
;; <fnc> ::= FNC num-vars (<and>)
(define fnc-list
    (lambda (num-vars and-list)
        (list 'FNC num-vars and-list)
    )
)

;; Ejemplos
(or-list '(1 2 3))
(and-list (list (or-list '(1 2 3)) (or-list '(1 3))))
(fnc-list 3 (and-list (list (or-list '(1 2 3)) (or-list '(1 3)))))


; ESTRACTORES

;; fnc-list->var :
;; Proposito: fnc -> n
;; Procedimiento que dado un fnc en sintaxis abstracta basada en listas,
;; devuelve el numero de variables de dicho fnc
;; <fnc> ::= FNC num-vars (<and>)
(define fnc-list->var
    (lambda (fnc)
        (cadr fnc)
    )
)

;; fnc-list->var :
;; Proposito: fnc -> 'clausulas
;; Procedimiento que dado un fnc en sintaxis abstracta basada en listas,
;; devuelve las clausulas, es decir el and en sintaxis abstracta, de dicho fnc
;; <fnc> ::= FNC num-vars (<and>)
(define fnc-list->clausulas
    (lambda (fnc)
        (cadr (caddr fnc))
    )
)

;; or-list :
;; Proposito: or -> L'
;; Procedimiento que dado un or en sintaxis abstracta basado en listas,
;; devuelve la lista de variables de dicho or
;; <or> ::= OR (<var> {<var>}*)
(define or-list->varlist
    (lambda (or)
        (cadr or)
    )
)

; Ejemplos
(fnc-list->var (fnc-list 3 (and-list (list (or-list '(1 2 3)) (or-list '(1 3))))))
(fnc-list->clausulas (fnc-list 3 (and-list (list (or-list '(1 2 3)) (or-list '(1 3))))))
(or-list->varlist (or-list '(1 2 3)))

; INSTANCIAS
(define SAT-list1 (fnc-list 3 (and-list (list (or-list '(1 2 3)) (or-list '(-1 -2)) (or-list '(-3))))))
(define SAT-list2 (fnc-list 4 (and-list (list (or-list '(-1 3)) (or-list '(1 -2)) (or-list '(-4)) (or-list '(2 3 4))))))
(define SAT-list3 (fnc-list 2 (and-list (list (or-list '(-1 2)) (or-list '(1 -2)) (or-list '(-2)) (or-list '(1 2))))))


;; Gramatica con DataTypes

;; <or> ::= OR (<var> {<var>}*)
(define-datatype or or?
    (or-vars
        (vars (list-of number?))
    )
)

;; <and> ::= AND (<or> {<or>}*) 
(define-datatype and and?
    (and-clauses
        (clauses (list-of or?))
    )
)

;; <fnc> ::= FNC num-vars (<and>)
(define-datatype sat sat?
    (fnc-exp
        (num number?)
        (exp and?)
    )
)

; Ejemplos
(or-vars '(1 2 3))
(and-clauses (list (or-vars '(1 2 3)) (or-vars '(-2 3))))
(fnc-exp 3 (and-clauses (list (or-vars '(1 2 3)) (or-vars '(-2 3)))))

; INSTANCIAS
(define SAT1 (fnc-exp 3 (and-clauses (list (or-vars '(1 2 3)) (or-vars '(-2 3))))))
(define SAT2 (fnc-exp 2 (and-clauses (list (or-vars '(1)) (or-vars '(-1 2)) (or-vars '(-2))))))
(define SAT3 (fnc-exp 3 (and-clauses (list (or-vars '(1 2 3)) (or-vars '(-2 3)) (or-vars '(1 -3))))))
