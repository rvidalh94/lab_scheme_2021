#lang racket

(require "tda_date.rkt")
(require "tda_user.rkt")

;TDA de account

;CONSTRUCTOR

;Función account
;Descripcion: Funcion que permite la creacion de una cuenta
;Dominio: string x string x entero x entero x entero x entero x entero x entero
;Recorrido: Lista con cuenta creada
;usuario, fecha, lista de publicaciones, lista de reacciones, lista de amigos, 1 sesion activa 0 sesion inactiva
(define account(lambda (username password dd mm yyyy hh m)
      (list (user username password) (date dd mm yyyy hh m) '() '() '() 0)
))


(provide account)