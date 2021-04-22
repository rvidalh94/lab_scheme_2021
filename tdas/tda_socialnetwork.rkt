#lang racket
(require "tda_user.rkt")
(require "tda_account.rkt")


;tda socialnetwork

;CONSTRUCTOR

;Funcion socialnetwork
;;Descripcion: funcion para crear una socialnetwork
;Dominio: string
;Recorrido: Lista con el nombre de la red social y una lista con las cuentas

(define socialnetwork (lambda (name)
  (list name '())
))


;SELECTORES

;get-socialname
;Descripcion: Funcion que obtiene el nombre de la red social
;Dominio: socialnetwork
;Recorrido: string

(define get-socialname (lambda (socialnetwork)
  (car socialnetwork)
))

;get-accounts
;Descripcion: Funcion que obtiene la lista de cuentas de la red social
;Dominio: socialnetwork
;Recorrido: list

(define get-accounts (lambda (socialnetwork)
  (car (cdr socialnetwork))
))


;MODIFICADORES

;mod-socialname
;Descripcion: Funcion para modificar el nombre de la red social
;Dominio: socialnetwork x string
;Recorrido: socialnetwork

(define mod-socialname (lambda (socialnetwork name)
      (cons name ((get-accounts socialnetwork)))
))


;OTRAS OPERACIONES

;register
;Funcion que registra a un usuario






