#lang eopl

;******************************************************************************************
;;;;; Interpretador Simple

;; La definición BNF para las expresiones del lenguaje:
;;
;;  <programa>          ::= <expression>
;;                          <un-programa (exp)>
;;  <expression>       ::= <numero>
;;                         <numero-lit (num)>
;;                     ::= "\""<texto>"\""
;;                         <texto-lit (txt)>
;;                     ::= <identificador>
;;                         <var-exp (id)>
;;                     ::= (<expresion> <primitiva-binaria> <expresion>)
;;                         primapp-bin-exp (exp1 prim-binaria exp2)
;;                     ::= <primitiva-unaria> (<expresion>)
;;                         primapp-un-exp (prim-unaria exp)
;;                     ::= Si <expresion> entonces <expresion> sino <expresion> finSI
;;                     ..= condicional-exp(test-exp true-exp false-exp)
;;
;; <primitiva-binaria> ::= + (primitiva-suma)
;;                     ::= ~ (primitiva-resta)
;;                     ::= * (primitiva-div)
;;                     ::= concat (primitiva-concat)
;;
;; <primitiva-unaria>  ::= longitud (primitiva-longitud)
;;                     ::= add1 (primitiva-add1)
;;                     ::= sub1 (primitiva-sub1)

;******************************************************************************************

;******************************************************************************************
;Especificación léxica
(define scanner-spec-simple-interpreter
  '((white-sp ;Espacios en blanco
     (whitespace) skip) 
    (comentario ;Comentarios
     ("//" (arbno (not #\newline))) skip)
    (texto
     (letter (arbno (or letter digit))) string)
    (identificador ;Identificadores
     ("@" (arbno letter)) symbol)
    (numero ;Número entero positivo
     (digit (arbno digit)) number)
    (numero ;Número entero negativo
     ("-" digit (arbno digit)) number)
    (numero
     (digit (arbno digit) "." digit (arbno digit)) number) ;Número decimal positivo
    (numero ;Número decimal negativo
     ("-" digit (arbno digit) "." digit (arbno digit)) number)
    ))

;Especificación Sintáctica (Gramática)
(define grammar-simple-interpreter
  '(;Programa
    (programa (expresion) un-programa)

    ;Expresiones
    (expresion (numero) numero-lit)
    (expresion ("\""texto"\"") texto-lit)
    (expresion (identificador) var-exp)
    (expresion
     ("("expresion primitiva-binaria expresion")") primapp-bin-exp)
    (expresion (primitiva-unaria "("expresion")") primapp-un-exp)
    ;Condicional
    (expresion ("Si" expresion "entonces" expresion "sino" expresion "finSI") condicional-exp)
    ;Variables Locales
    (expresion ("declarar" "("  (separated-list identificador "=" expresion ";") ")" "{" expresion "}") variableLocal-exp)
    ;Procedimientos
    (expresion ("procedimiento" "(" (separated-list identificador ",") ")" "haga" expresion "finProc" ) procedimiento-exp)
    
    ;Primitivas-binarias
    (primitiva-binaria ("+") primitiva-suma)
    (primitiva-binaria ("~") primitiva-resta)
    (primitiva-binaria ("/") primitiva-div)
    (primitiva-binaria ("*") primitiva-multi)
    (primitiva-binaria ("concat") primitiva-concat)

    ;Primitivas-unarias
    (primitiva-unaria ("longitud") primitiva-longitud)
    (primitiva-unaria ("add1") primitiva-add1)
    (primitiva-unaria ("sub1") primitiva-sub1)
    
    ))



;Construyendo datos automáticamente
(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

;*******************************************************************************************
;Parser, Scanner, Interfaz

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Interpretador (FrontEnd + Evaluación + Señal para lectura)
(define interpretador
  (sllgen:make-rep-loop "--> "
    (lambda (pgm) (eval-program  pgm))
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))

;*******************************************************************************************
;El Interprete

;eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define eval-program
  (lambda (pgm)
    (cases programa pgm
      (un-programa (body)
                   (eval-expresion body (init-env))))))

; Ambiente inicial
(define init-env
  (lambda ()
    (extend-env
     '(@a @b @c @d @e)
     '(1 2 3 "hola" "FLP")
     (empty-env))))

;eval-expresion: <expresion> <environment> -> numero
; evalua la expresion en el ambiente de entrada
(define eval-expresion
  (lambda (exp env)
    (cases expresion exp
      (numero-lit (num) num)
      (texto-lit (txt) txt)
      (var-exp (id) (buscar-variable env id))
      (primapp-bin-exp (exp1 prim-binaria exp2)
                       (let (
                             (args1 (eval-rand exp1 env))
                             (args2 (eval-rand exp2 env))
                             )
                         (apply-primitiva-binaria args1 prim-binaria args2)))
      (primapp-un-exp (prim-unaria exp)
                      (let (
                            (args (eval-rand exp env))
                            )
                        (apply-primitiva-unaria prim-unaria args)))
      (condicional-exp (test-exp true-exp false-exp)
                       (if (valor-verdad? (eval-expresion test-exp env))
                           (eval-expresion true-exp env)
                           (eval-expresion false-exp env)))
      (variableLocal-exp (ids exps cuerpo)
                          (let ((args (eval-rands exps env )))
                           (eval-expresion cuerpo (extend-env ids args env))
                           ))
       (procedimiento-exp (ids cuerpo)
        (cerradura ids cuerpo env)
      )
      )))


; Funcion auxiliar para aplicar eval-rand a cada elemento dentro de exp1 exp2
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expresion rand env)))

;apply-primitiva-binaria: <expresion> <primitiva> <expresion> -> numero or string
(define apply-primitiva-binaria
  (lambda (exp1 prim exp2)
    (cases primitiva-binaria prim
      (primitiva-suma () (+ exp1 exp2))
      (primitiva-resta () (- exp1 exp2))
      (primitiva-multi () (* exp1 exp2))
      (primitiva-div () (/ exp1 exp2))
      (primitiva-concat () (string-append exp1 exp2))
      )))

;apply-primitiva-unaria <primitiva> <expresion> -> numero
(define apply-primitiva-unaria
  (lambda (prim exp)
    (cases primitiva-unaria prim
      (primitiva-longitud () (string-length exp))
      (primitiva-add1 () (+ exp 1))
      (primitiva-sub1 () (- exp 1))
      )))

;valor-verdad? <expresion> -> Boolean
(define valor-verdad?
  (lambda(x)
    (not (zero? x))))



;*******************************************************************************************
;Ambientes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (syms (list-of symbol?))
                       (vals (list-of scheme-value?))
                       (env environment?)))

(define scheme-value? (lambda (v) #t))

;empty-env:      -> enviroment
;función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record)))       ;llamado al constructor de ambiente vacío 


;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env))) 

;función que busca un símbolo en un ambiente
(define buscar-variable
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'buscar-variable "Error, la variable no existe" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (buscar-variable env sym)))))))

(define-datatype procVal procVal?
  (cerradura
   (lista-ID (list-of symbol?))
   (exp expresion?)
   (amb environment?)
   )
  )

;****************************************************************************************
;Funciones Auxiliares

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de unambiente

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

;******************************************************************************************
(interpretador)