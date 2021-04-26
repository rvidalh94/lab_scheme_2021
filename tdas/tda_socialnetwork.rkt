#lang racket
(require "tda_user.rkt")
(require "tda_date.rkt")


;tda socialnetwork

;CONSTRUCTOR

;Funcion socialnetwork
;;Descripcion: funcion para crear una socialnetwork
;Dominio: string
;Recorrido: Lista con el nombre de la red social, lista de pares usuario x fecha, listad de publicaciones y lista de reacciones

(define socialnetwork (lambda (name date encryptFn decryptFn)
  (list name date null null null)
))


; PERTENENCIA
;funcion socialnetwork?
;funcion para validar que es un socialnetwork
;Dominio: valor
;Recorrido: true o false depende de la validacion

(define socialnetwork? (lambda (sn)
    (if (list? sn)
        (if (= (length sn) 5)
            (if (and (string? (car sn)) (date? (car (cdr sn))) (list? (car (cdr (cdr sn))))
                     (list? (car (cdr (cdr (cdr sn))))) (list? (car (cdr (cdr (cdr (cdr sn)))))))
                #t
                #f
            )
            #f
         )
        #f
     )
))


;SELECTORES

;funcion para seleccionar nombre de red social
;Dominio: socialnetwork
;Recorrido: string
(define get-social-name (lambda (s)
   (car s)
))

;funcion para seleccionar fecha de creacion
;Dominio: socialnetwork
;Recorrido: date
(define get-social-date (lambda (s)
   (car (cdr s))
))

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
   (car (cdr (cdr (cdr (cdr s)))))
))


;MODIFICADORES

;update-user-list
;funcion que actualiza la lista de usuarios
;Dominio: socialnetwork x lista
;recorrido: socialnetwork

(define update-user-list (lambda (sn nl)
    (list (get-social-name sn) (get-social-date sn) nl (get-pub-list sn) (get-react-list sn))
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
(provide socialnetwork?)
(provide update-user-list)

