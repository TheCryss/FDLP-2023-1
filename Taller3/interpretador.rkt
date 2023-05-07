#lang eopl

; Estudiantes:
; Jose Luis Hincapie Bucheli - 2125340
; Sebatian Idrobo Avirama - 2122637
; Juan Sebastian Getial Getial - 2124644

;******************************************************************************************
;Especificación Léxica

(define scanner-spec-simple-interpreter
  '(
    (white-sp
      (whitespace) skip
    )
    (comment
      ("%" (arbno (not #\newline))) skip
    )
    (identificador
      ("@" letter (arbno (or letter digit "?"))) symbol
    )
    (numero
      (digit (arbno digit)) number
    )
    (numero
      ("-" digit (arbno digit)) number
    )
    (numero
      (digit (arbno digit) "." digit (arbno digit)) number
    )
    (numero
      ("-" digit (arbno digit) "." digit (arbno digit)) number
    )
    (texto
      (letter (arbno (or letter digit))) string
    )
  )
)
;*******************************************************************
;Especificación Sintáctica (gramática)

(define grammar-simple-interpreter
  '(
    (programa (expresion) un-programa)
    (expresion (numero) numero-lit)
    (expresion ("\"" texto "\"") texto-lit)
    (expresion (identificador) var-exp)
    (expresion ("(" expresion primitiva-binaria expresion ")") primapp-bin-exp)
    (expresion (primitiva-unaria "(" expresion ")") primapp-un-exp)
    (expresion ("Si" expresion "entonces" expresion "sino" expresion "finSI") condicional-exp)
    (expresion ("declarar" "(" (separated-list identificador "=" expresion ";") ")" "{" expresion "}") variableLocal-exp)
    (expresion ("procedimiento" "(" (separated-list identificador ",") ")" "haga" expresion "finProc") procedimiento-exp)
    (expresion ("evaluar" expresion "(" (separated-list expresion ",") ")" "finEval") app-exp)
    (expresion ("letrec" (arbno identificador "(" (separated-list identificador ",") ")" "=" expresion)  "in" expresion) 
                letrec-exp)

    ; ; Primitivas Binarias
    (primitiva-binaria ("+") primitiva-suma)
    (primitiva-binaria ("~") primitiva-resta)
    (primitiva-binaria ("/") primitiva-div)
    (primitiva-binaria ("*") primitiva-multi)
    (primitiva-binaria ("concat") primitiva-concat)

    ; Primitivas Unarias
    (primitiva-unaria ("longitud") primitiva-longitud)
    (primitiva-unaria ("add1") primitiva-add1)
    (primitiva-unaria ("sub1") primitiva-sub1)
  )
)

;*******************************************************************
;Tipos de datos para la sintaxis abstracta de la gramática
;Construidos automáticamente:

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

;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program  pgm)) 
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))


;*******************************************************************************************
;El Interprete

; eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro 
; del programa)

(define eval-program
  (lambda (pgm)
    (cases programa pgm
      (un-programa (body)
                 (evaluar-expresion body (init-env))))))

(define init-env
  (lambda ()
    (extend-env
     '(@a @b @c @d @e)
     (list 1 2 3 "hola" "FLP")
     (empty-env))))

;evaluar-expresion: <expression> <enviroment> -> numero
; evalua la expresión en el ambiente de entrada
(define evaluar-expresion
  (lambda (exp env)
    (cases expresion exp
      (numero-lit (num) num)
      (texto-lit (text) text)
      (var-exp (id) (buscar-variable env id))
      (primapp-bin-exp (exp1 prim-bin exp2)
        (aplicar-primitiva-binaria 
          prim-bin 
          (evaluar-expresion exp1 env)
          (evaluar-expresion exp2 env)
        )
      )
      (primapp-un-exp (prim-un exp)
        (aplicar-primitiva-unaria
          prim-un
          (evaluar-expresion exp env)
        )
      )
      (condicional-exp (test-exp true-exp false-exp)
        (if (valor-verdad? (evaluar-expresion test-exp env))
          (evaluar-expresion true-exp env)
          (evaluar-expresion false-exp env)
        )
      )
      (variableLocal-exp (ids exps cuerpo)
        (let ((args (eval-rands exps env)))
          (evaluar-expresion 
            cuerpo
            (extend-env ids args env)
          )
        )
      )
      (procedimiento-exp (ids cuerpo)
        (cerradura ids cuerpo env)
      )
      (app-exp (rator rands)
        (let 
          (
            (proc (evaluar-expresion rator env))
            (args (eval-rands rands env))
          )
          (if (procVal? proc)
            (apply-procedure proc args)
            (eopl:error 'eval-expression
                                 "Attempt to apply non-procedure ~s" proc)
          )
        )
      )
      (letrec-exp (proc-names idss bodies letrec-body)
                  (evaluar-expresion letrec-body
                                   (extend-env-recursively proc-names idss bodies env)))
    )
  )
)

; aplicar-primitiva-binaria: <primitiva> arg1 arg2 -> numero
(define aplicar-primitiva-binaria
  (lambda (prim-bin arg1 arg2)
    (cases primitiva-binaria prim-bin
      (primitiva-suma () (+ arg1 arg2))
      (primitiva-resta () (- arg1 arg2))
      (primitiva-div () (/ arg1 arg2))
      (primitiva-multi () (* arg1 arg2))
      (primitiva-concat () (string-append arg1 arg2))
    )
  )
)

; aplicar-primitiva-unaria: <primitiva> arg1 arg2 -> numero
(define aplicar-primitiva-unaria
  (lambda (prim-un arg)
    (cases primitiva-unaria prim-un
      (primitiva-longitud () (string-length arg))
      (primitiva-add1 () (+ arg 1))
      (primitiva-sub1 () (- arg 1))
    )
  )
)

; valor-verdad?: determina si un valor dado corresponde a un valor booleano falso o verdadero
(define valor-verdad?
  (lambda (x)
    (not (zero? x))))

;*****************************************************
; procVal de un procedemiento
(define-datatype procVal procVal?
  (cerradura
    (lista-ID (list-of symbol?))
    (exp expresion?)
    (amb environment?)
  )
)

;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args)
    (cases procVal proc
      (cerradura (ids body env)
               (evaluar-expresion body (extend-env ids args env))))))

;*******************************************************************************************
;Ambientes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (syms (list-of symbol?))
                       (vals (list-of scheme-value?))
                       (env environment?))
  (recursively-extended-env-record (proc-names (list-of symbol?))
                                   (idss (list-of (list-of symbol?)))
                                   (bodies (list-of expresion?))
                                   (env environment?))
)

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

;extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;función que crea un ambiente extendido para procedimientos recursivos
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (recursively-extended-env-record
     proc-names idss bodies old-env)))

;función que busca un símbolo en un ambiente
(define buscar-variable
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error "Error, la variable no existe"))
      (extended-env-record (syms vals env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (buscar-variable env sym))))
      (recursively-extended-env-record (proc-names idss bodies old-env)
                                       (let ((pos (list-find-position sym proc-names)))
                                         (if (number? pos)
                                             (cerradura (list-ref idss pos)
                                                      (list-ref bodies pos)
                                                      env)
                                             (buscar-variable old-env sym)))))))

; funciones auxiliares para aplicar evaluar-expresion a cada elemento de una 
; lista de operandos (expresiones)
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (evaluar-expresion rand env)))


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