#lang racket

(require "tda_socialnetwork.rkt")
(require "tda_date.rkt")
(require "tda_user.rkt")
(require "tda_publication.rkt")


;Funcion register
;Funcion para registrar a un usuario en una red social
;Dominio: socialnetwork x date x string x string
;Recorrido: socialnetwork

(define register (lambda (sn dt username password)
     (if (and (socialnetwork? sn) (date? dt) (string? username) (string? password))
         (if (null? (get-user (get-user-list sn) username))
             (update-user-list sn (add-user (get-user-list sn) (user username password dt)))
             sn
          )
          null
      )
))


;(define face (socialnetwork "Facebook" (date 01 02 2004) encrypt decrypt))
;(define face(register face (date 25 04 2021) "bati" "123455"))
;(define face(register face (date 01 04 2021) "Rodrigo" "aaaa"))
;(define face(register face (date 01 04 2021) "stefane" "bbb"))


;Funcion login
;Usuario currificada que permite al usuario iniciar sesion en socialnetwork
;Dominio: socialnetwork x string x string x operation
;Recorrido: funcion

(define login (lambda (sn username password operation)
        (let ([usr (get-user (get-user-list sn) username)])
          (if (null? usr)
              operation
              (if (and (eqv? (get-username usr) username) (eqv? (get-password usr) password))
                  (operation (update-logged-user sn username))
                  operation
              )
          )
        )       
))



;funcion post
;funcion que agrega una publicacion al socialnetwork
;dominio: socialnetwork
;recorrido: socialnetwork

;(define post (lambda (sn)
 ;              (lambda (dt)
  ;               (lambda (b userlist)
                   
   ;            )))
;)