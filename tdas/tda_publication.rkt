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




;add-publication
;Funcion para agregar una publicacion
;Dominio: lista x publication
;Recorrido: lista
(define pID car)
(define lastPID car)

(define add-publication (lambda (qL usrn b dt)
                      (let ([nQ (publication (+ (lastPID qL) 1) usrn b dt)])
                        (cons (pID nQ) (cons nQ (cdr qL))))))



(provide publication)



