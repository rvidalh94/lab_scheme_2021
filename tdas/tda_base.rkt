#lang racket

(require "tda_socialnetwork.rkt")
(require "tda_date.rkt")
(require "tda_user.rkt")
(require "tda_account.rkt")
(require "tda_publication.rkt")
(require "otras_funciones.rkt")


;FUNCION REGISTER
;Descripción: Función que permite el registro de un usuario en una red social.
;Dominio: socialnetwork x date x string x string
;Recorrido: socialnetwork

(define register (lambda (sn dt username password)
     (if (and (socialnetwork? sn) (date? dt) (string? username) (string? password))
         (if (null? (get-account (get-account-list sn) username))
             (list (get-social-name sn) (get-social-date sn) (add-account (get-account-list sn) (user username password) dt) "")
             sn
          )
          null
      )
))

; EJEMPLOS DE PRUEBA PARA REGISTER
;(define face (socialnetwork "Facebook" (date 01 02 2004) encrypt decrypt)) #se crea red social
;(define face(register face (date 25 04 2021) "rodrigo" "123455")) # se agrega primer usuario
;(define face(register face (date 01 04 2021) "stefane" "aaaa")) # se agrega segundo usuario
;(define face(register face (date 01 04 2021) "rodrigo" "bbb")) # se agrega usuario existente (username)
;(define face(register face (date 01 04 2021) "Martina" "aaaa")) # se agrega tercer usuario

;-------------------------------------------------------------------------------------------------

;FUNCION LOGIN
;Descripción: Función que permite a un usuario iniciar sesion en una red social.
;Dominio: socialnetwork x string x string x operation
;Recorrido: funcion

(define login (lambda (sn username password operation)
        (let ([usr (get-user (get-account-list sn) username)])
          (if (null? usr)
              operation
              (if (and (eqv? (get-username usr) username) (eqv? (get-password usr) password))
                  (operation (update-logged-user sn username))
                  operation
              )
          )
        )       
))

;EJEMPLOS DE PRUEBA PARA LOGIN
;(define face (socialnetwork "Facebook" (date 01 02 2004) encrypt decrypt))
;(define face(register face (date 01 04 2021) "rodrigo" "12345"))
;(define face(register face (date 12 09 2021) "stefane" "abcde"))
;(login face "rodrigo" "12345" null)


;-------------------------------------------------------------------------------------------------


;FUNCION POST
;Descripción: Función que permite a un usuario realizar un post en su muro o en el de otros usuarios.
;Dominio: socialnetwork
;Recorrido: funcion

(define post (lambda (sn)
               (lambda (dt)
                 (lambda (content . users)
                    (let ([loggeduser (get-user (get-account-list sn) (get-logged-user sn))])
                       (if (null? users)
                           (f0 sn (get-username loggeduser) dt content)
                           (if (check-friends (get-account (get-account-list sn) (get-username loggeduser)) users); solo postea si TODOS sin amigos de user.
                               (car (map (lambda (x) (f0 sn x dt content (get-username loggeduser))) users))
                               sn
                           )                           
                        )                       
                     ) 
                )))
)

;FUNCIONES AUXILIARES PARA POST
(define f0 (lambda (sn usr dt content . user-from)
     (list (get-social-name sn) (get-social-date sn)
           (f1 usr (get-account-list sn) content dt user-from) "")
))


(define f1 (lambda (usr account-list body dt . user-from)
      (if (null? user-from)
          (update-account usr (update-account-publication (get-account account-list usr)
          (add-publication (get-account-pub (get-account account-list usr)) usr body dt)) account-list null)
          (update-account usr (update-account-publication (get-account account-list usr)
          (add-publication (get-account-pub (get-account account-list usr)) user-from body dt)) account-list null) 
       )      
))

;EJEMPLOS DE PRUEBA PARA POST
;(define face (socialnetwork "Facebook" (date 01 02 2004) encrypt decrypt))
;(define face(register face (date 01 04 2021) "rodrigo" "12345"))
;(define face(register face (date 12 09 2021) "stefane" "abcde"))
;(define face1 (((login face "rodrigo" "12345" post) (date 30 10 2020)) "primer post"))
;(define face2 (((login face1 "rodrigo" "12345" post) (date 30 10 2020)) "segundo post"))
;(define face3 (((login face1 "rodrigo" "12345" post) (date 30 10 2020)) "segundo post" "stefane"))

;-------------------------------------------------------------------------------------------------


;FUNCION FOLLOW
;Descripción: Función que permite agregar un usuario a la lista de amigos de otro usuario, retorna socialnetwork y cierra sesion.
;dominio: socialnetwork x date x string
;recorrido: socialnetwork

(define follow (lambda (sn)
                 (lambda (dt)
                   (lambda (usr)
                     (if (not (eqv? (get-logged-user sn) usr))
                         (let ([loggeduser (get-user (get-account-list sn) (get-logged-user sn))])                          
                           (if (exist-friend (get-account-friends (get-account (get-account-list sn) (get-username loggeduser))) usr)
                               sn
                               (list (get-social-name sn)
                                     (get-social-date sn)
                                     (update-account (get-username loggeduser) (update-account-friends
                                     (get-account (get-account-list sn) (get-username loggeduser))
                                     (appendPropio (get-account-friends (get-account (get-account-list sn) (get-username loggeduser))) usr))
                                     (get-account-list sn) null)
                                     
                               "")
                            ) 
                          ) 
                         sn
                      ))))
)

;EJEMPLOS DE PRUEBA PARA FOLLOW
;(define face (socialnetwork "Facebook" (date 01 02 2004) encrypt decrypt))
;(define face(register face (date 01 04 2021) "rodrigo" "12345"))
;(define face(register face (date 12 09 2021) "stefane" "abcde"))
;(define face(register face (date 05 08 2021) "bastian" "basti123"))
;(define face(register face (date 15 04 2021) "martina" "gatitos"))


;(define f1 (((login face "rodrigo" "12345" follow) (date 30 10 2020)) "stefane"))
;(define f2 (((login f1 "stefane" "abcde" follow) (date 30 10 2020)) "rodrigo"))
;(define f3 (((login f2 "stefane" "abcde" follow) (date 30 10 2020)) "martina"))
;(define f4 (((login f3 "rodrigo" "12345" follow) (date 30 10 2020)) "bastian"))



;-------------------------------------------------------------------------------------------------
