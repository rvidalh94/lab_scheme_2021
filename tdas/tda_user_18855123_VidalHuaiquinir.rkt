#lang racket
(require "tda_date_18855123_VidalHuaiquinir.rkt")

;TDA USER


;CONSTRUCTOR

;Función user
;Descripción: Función que permite la creación de un usuario.
;Dominio: string x string
;Recorrido: lista

(define user (lambda (username password)
    (if (and (string? username) (string? password))
        (list username password)
         null
     )
))


;SELECTORES

;Función get-username
;Descripción: Función que retorna el username del user.
;Dominio: user
;Recorrido: string

(define get-username (lambda (user)
  (car user)
))

;Función get-password
;Descripción: Función retorna el password del user.
;Dominio: user
;Recorrido: string
(define get-password (lambda (user)
  (car (cdr user))
))


(provide user)
(provide get-username)
(provide get-password)