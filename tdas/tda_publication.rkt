#lang racket
(require "tda_date.rkt")
;TDA publication

;CONSTRUCTOR

;Funcion publication
;Descripción: Función que permite  crear una publicacion
;Dominio: entero x string x string x string x date
;Recorrido: lista con la publicacion creada

(define publication (lambda (pubId user body datep)
     (list pubId user  body datep 0 0)
))




;add-publication
;Funcion para agregar una publicacion
;Dominio: lista x publication
;Recorrido: lista
(define pID car)
(define lastPID car)

(define add-publication (lambda (publication-list usr body dt)
                      (let ([nQ (publication (+ (lastPID publication-list) 1) usr body dt)])
                        (cons (pID nQ) (cons nQ (cdr publication-list))))))




(provide publication)
(provide add-publication)