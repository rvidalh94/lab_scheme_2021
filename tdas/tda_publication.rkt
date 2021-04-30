#lang racket
(require "tda_date.rkt")

;TDA publication

;CONSTRUCTOR

;Funcion publication
;Descripción: Función que permite  crear una publicacion
;Dominio: entero x string x string x string x date
;Recorrido: lista con la publicacion creada

(define publication (lambda (pubId user-to user-from body datep)
     (list pubId user-to user-from  body datep 0 0)
))




;add-publication
;Funcion para agregar una publicacion
;Dominio: lista x publication
;Recorrido: lista
(define pID car)
(define lastPID car)

(define add-publication (lambda (qL usrto usrfrom b dt)
                      (let ([nQ (publication (+ (lastPID qL) 1) usrto usrfrom b dt)])
                        (cons (pID nQ) (cons nQ (cdr qL))))))




(provide publication)