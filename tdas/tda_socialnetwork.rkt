#lang racket
(require "tda_user.rkt")
(require "tda_date.rkt")
(require "tda_account.rkt")
(require "otras_funciones.rkt")


;TDA SOCIALNETWORK

;CONSTRUCTOR

;Funcion socialnetwork
;Descripción: Función para crear una socialnetwork
;Dominio: string x date x funcion x funcion
;Recorrido: Lista con el nombre de la red social, lista de pares usuario x fecha, listad de publicaciones y lista de reacciones

(define socialnetwork (lambda (name date encryptFn decryptFn)
  (list name date null "" encryptFn decryptFn 0)
))


; PERTENENCIA
;funcion socialnetwork?
;Descripción: Función para validar que es un socialnetwork.
;Dominio: valor
;Recorrido: true o false depende de la validacion

(define socialnetwork? (lambda (sn)
    (if (list? sn)
        (if (= (length sn) 7)
            (if (and (string? (car sn)) (date? (car (cdr sn))) (list? (car (cdr (cdr sn))))
                     (string? (car (cdr (cdr (cdr sn))))))
                #t
                #f
            )
            #f
         )
        #f
     )
))


;SELECTORES

;get-social-name
;Descripción: Función para obtener nombre de red social.
;Dominio: socialnetwork
;Recorrido: string
(define get-social-name (lambda (s)
   (car s)
))

;get-social-date
;funcion para obtener la fecha de creacion de una red social.
;Dominio: socialnetwork
;Recorrido: date
(define get-social-date (lambda (s)
   (car (cdr s))
))

;get-account-list
;Descripción: Función para obtener la lista de cuentas de una red social.
;Dominio: socialnetwork
;Recorrido: lista
(define get-account-list (lambda (s)
   (car (cdr (cdr s)))
))

;get-logged-user
;Descripción: Función para obtener el username del usuario en sesion activa en una red social.
;Dominio: socialnetwork
;Recorrido: string
(define get-logged-user (lambda (s)
   (car (cdr (cdr (cdr s))))
))

;get-encrypt
;Descripción: Función que optiene la funcion de encriptar en la red social.
;Dominio: socialnetwork
;Recorrido: function
(define get-encrypt (lambda (s)
   (car (cdr (cdr (cdr (cdr s)))))
))

;get-decrypt
;Descripción: Función que optiene la funcion de desencriptar en la red social.
;Dominio: socialnetwork
;Recorrido: function
(define get-decrypt (lambda (s)
   (car (cdr (cdr (cdr (cdr (cdr s))))))
))


;get-last-pubID
;Descripción: Función que optiene la funcion de desencriptar en la red social.
;Dominio: socialnetwork
;Recorrido: function
(define get-last-pubID (lambda (s)
   (car (cdr (cdr (cdr (cdr (cdr (cdr s)))))))
))


;MODIFICADORES

;update-account-list
;Descripción: Función que actualiza la lista de usuarios de una red social.
;Dominio: socialnetwork x lista
;recorrido: socialnetwork

(define update-user-list (lambda (sn nl)
    (list (get-social-name sn) (get-social-date sn) nl (get-logged-user sn) (get-encrypt sn) (get-decrypt sn) (get-last-pubID sn))
))


;update-logged-user
;Descripción: Función que modifica el username del usuario activo en sesion de una red social.
;dominio: socialnetwork x string
;recorrido: socialnetwork

(define update-logged-user (lambda (sn username)
     (list (get-social-name sn) (get-social-date sn) (get-account-list sn) username (get-encrypt sn) (get-decrypt sn) (get-last-pubID sn))
))



(provide socialnetwork)
(provide socialnetwork?)
(provide encrypt)
(provide decrypt)
(provide update-user-list)
(provide update-logged-user)
(provide get-account-list)
(provide get-logged-user)
(provide get-social-name)
(provide get-social-date)
(provide get-encrypt)
(provide get-decrypt)
(provide get-last-pubID)