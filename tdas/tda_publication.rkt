#lang racket
(require "tda_date.rkt")

;TDA publication

;CONSTRUCTOR

;Funcion publication
;Descripcion: Funcion que permite  crear una publicacion
;Dominio: entero x user x string x date
;Recorrido: lista con la publicacion creada

(define publication (lambda (pubId username body datep)
     (list pubId username body datep 0 0)
))





