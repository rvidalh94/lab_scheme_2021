#lang racket
(require "tda_user.rkt")
(require "tda_date.rkt")


;tda socialnetwork

;CONSTRUCTOR

;Funcion socialnetwork
;;Descripcion: funcion para crear una socialnetwork
;Dominio: string
;Recorrido: Lista con el nombre de la red social y una lista con las cuentas

(define socialnetwork (lambda (name date encryptFn decryptFn)
  (list name date null null null)
))


; PERTENENCIA
;funcion socialnetwork?
;funcion para validar que es un socialnetwork
;



;SELECTORES

;funcion para seleccionar lista de usuarios
;Dominio: socialnetwork
;Recorrido: lista de usuarios
(define get-user-list (lambda (s)
   (car (cdr (cdr s)))
))

;funcion para seleccionar lista de publicaciones
;Dominio: socialnetwork
;Recorrido: lista de publicaciones
(define get-pub-list (lambda (s)
   (car (cdr (cdr (cdr s))))
))


;funcion para seleccionar lista de reacciones
;Dominio: socialnetwork
;Recorrido: lista de reacciones
(define get-react-list (lambda (s)
   (car (cdr (cdr (cdr (s)))))
))



;Otras funciones

;Funcion encrypt
;Funcion que encripta un texto inviertiendo su contenido
;Dominio: string
;Recorrido: string
(define encrypt (lambda (content)
     (list->string (reverse (string->list content)))
))


;Funcion decrypt
;Funcion que desencripta un texto inviertiendo su contenido
;Dominio: string
;Recorrido: string
(define decrypt (lambda (content)
     (list->string (reverse (string->list content)))
))



(provide socialnetwork)
(provide get-user-list)
(provide encrypt)
(provide decrypt)



