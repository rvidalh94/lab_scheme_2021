#lang racket
(require "otras_funciones.rkt")

;TDA publication

;CONSTRUCTOR

;Funcion publication
;Descripción: Función que permite  crear una publicacion
;Dominio: entero x string x string x string x date
;Recorrido: lista con la publicacion creada

(define publication (lambda (pubId user body datep pubOrgID)
     (list pubId user  body datep 0 0 pubOrgID)
))


;SELECTORES


;get-publication-id
(define get-publication-id (lambda (pub)
       (car pub)
))


;get-publication-content
(define get-publication-content (lambda (pub)
       (car (cdr (cdr pub)))
))

;get-publication
;Descripción: Función que permite obtener una publicacion mediante el ID de la publicacion
;Dominio: lista
;Recorrido: publication

(define get-publication (lambda (publication-list pID)
     (if (null? publication-list)
         null
         (if (= (get-publication-id (car publication-list)) pID)
             (car publication-list)
             (get-publication (cdr publication-list) pID)
          )
      )
))


;OTRAS FUNCIONES

;add-publication
;Funcion para agregar una publicacion
;Dominio: lista x publication
;Recorrido: lista de publicaciones.
(define lastPID car)

(define add-publication (lambda (pID publication-list usr body dt pubOrgID)
                      (let ([nP (publication pID usr body dt pubOrgID)])
                        (appendPropio publication-list nP)
                        )))




(provide publication)
(provide add-publication)
(provide get-publication)
(provide get-publication-id)
(provide get-publication-content)