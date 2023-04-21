#lang eopl

; Estudiantes:
; Jose Luis Hincapie Bucheli - 2125340
; Sebatian Idrobo Avirama - 2122637
; Juan Sebastian Getial Getial - 2124644


;;; 1 Instancias SAT

;; 1.1 Gramatica BNF

; Gramatica FNC
; <fnc> ::= FNC num-vars (<and>)
; <and> ::= <or> {'and' <or>}* 
; <or> ::= <var> {'or' <var>}*
; <var> ::= 1 | -1 | 2 | -2 | ... | number-vars | -number-vars

;; Gramatica con listas

; CONSTRUCTORES

(define or-list
    (lambda (vars-list)
        (list 'OR vars-list)
    )
)

(define and-list
    (lambda (ors-list)
        (list 'AND ors-list)
    )
)

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

(define fnc-list->var
    (lambda (fnc)
        (cadr fnc)
    )
)

(define fnc-list->clausulas
    (lambda (fnc)
        (cadr (caddr fnc))
    )
)

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

(define-datatype or or?
    (or-vars
        (vars (list-of number?))
    )
)

(define-datatype and and?
    (and-clauses
        (clauses (list-of or?))
    )
)

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
