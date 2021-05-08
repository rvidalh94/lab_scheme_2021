#lang racket

;TDA REACTION

;CONSTRUCTOR

;Funcion reaction
;Descripcion: Funcion que permite  crear una reaccion
;Dominio: entero x user x string x date
;Recorrido: lista con la reaccion creada

(define reaction (lambda (reactId pubId username body datep)
     (list reactId pubId username body datep 0 0)
))
