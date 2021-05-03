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
;Método get-account utiliza recursión lo cual esta descrito en su definición.

(define register (lambda (sn dt username password)
     (if (and (socialnetwork? sn) (date? dt) (string? username) (string? password))
         (if (null? (get-account (get-account-list sn) username))
             (list (get-social-name sn) (get-social-date sn) (add-account (get-account-list sn) (user username password) dt) ""
                   (get-encrypt sn) (get-decrypt sn) (get-last-pubID sn))
             sn
          )
          null
      )
))

; EJEMPLOS DE PRUEBA PARA REGISTER

;(define face (socialnetwork "Facebook" (date 01 02 2004) encrypt decrypt)) - se crea red social
;(define face(register face (date 25 04 2021) "rodrigo" "123455")) - se agrega primer usuario
;(define face(register face (date 01 04 2021) "stefane" "aaaa")) - se agrega segundo usuario
;(define face(register face (date 01 04 2021) "rodrigo" "bbb")) - se agrega usuario existente (username)
;(define face(register face (date 01 04 2021) "martina" "aaaa")) - se agrega tercer usuario


;-------------------------------------------------------------------------------------------------


;FUNCION LOGIN

;Descripción: Función que permite a un usuario iniciar sesion en una red social.
;Dominio: socialnetwork x string x string x operation
;Recorrido: funcion
;Recorrido: socialnetwork
;Método get-user utiliza recursión lo cual esta descrito en su definición.

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

;(define face (socialnetwork "Facebook" (date 01 02 2004) encrypt decrypt)) - Crear una socialnetwork
;(define face (register face (date 01 04 2021) "rodrigo" "12345")) - Registrar usuario.
;(define face (register face (date 12 09 2021) "stefane" "abcde")) - Registar usuario.

;(login face "rodrigo" "12345" reverse) - Usuario existente. Se utiliza funcion reverse solo para probar que la funcion se aplica.
;(login face "stefane" "abcde" reverse) - Usuario existente. Se utiliza funcion reverse solo para probar que la funcion se aplica.
;(login face "martina" "12345" reverse) - Usuario no existente. Debe retonrar reverse, ya que al no estar logeado retorna solo la función dada.


;-------------------------------------------------------------------------------------------------


;FUNCION FOLLOW

;Descripción: Función que permite agregar un usuario a la lista de amigos de otro usuario, retorna socialnetwork y cierra sesion.
;dominio: socialnetwork
;recorrido: date x user
;recorrido: socialnetwork
;Método get-user utiliza recursión lo cual esta descrito en su definición.
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
                                     
                               "" (get-encrypt sn) (get-decrypt sn) (get-last-pubID sn))
                            ) 
                          ) 
                         sn
                      ))))
)


;EJEMPLOS DE PRUEBA PARA FOLLOW

;(define face (socialnetwork "Facebook" (date 01 02 2004) encrypt decrypt))  - Crea socialnetwork.
;(define face(register face (date 01 04 2021) "rodrigo" "12345")) - Registra un usuario.
;(define face(register face (date 12 09 2021) "stefane" "abcde")) - Registra un usuario.
;(define face(register face (date 05 08 2021) "bastian" "basti123")) - Registra un usuario.
;(define face(register face (date 15 04 2021) "martina" "gatitos")) - Registra un usuario.


;(define face (((login face "rodrigo" "12345" follow) (date 30 10 2020)) "stefane")) - Se sigue a usuario. Se asume que usuario existe.
;(define face (((login face "stefane" "abcde" follow) (date 30 10 2020)) "rodrigo")) - Se sigue a usuario. Se asume que usuario existe.
;(define face (((login face "stefane" "abcde" follow) (date 30 10 2020)) "martina")) - Se sigue a usuario. Se asume que usuario existe.
;(define face (((login face "rodrigo" "12345" follow) (date 30 10 2020)) "rodrigo")) - Se intenta seguir a si mismo.



;-------------------------------------------------------------------------------------------------



;FUNCION POST

;Descripción: Función que permite a un usuario realizar un post en su muro o en el de otros usuarios.
;Dominio: socialnetwork
;Recorrido: date x string x list
;Recorrido: socialnetwork
;Método post-to-users utiliza recursión lo cual esta descrito en su definición.

(define post (lambda (sn)
               (lambda (dt)
                 (lambda (content . users)
                    (let ([loggeduser (get-user (get-account-list sn) (get-logged-user sn))])
                       (if (null? users)
                           (f-aux1 sn (get-username loggeduser) dt content users 0)                             
                           (post-to-users sn (only-friend-list (get-account (get-account-list sn) (get-username loggeduser)) users null)
                                          dt content (get-username loggeduser) 0)                          
                        )                       
                     ) 
                )))
)



;EJEMPLOS DE PRUEBA PARA POST

;(define face (socialnetwork "Facebook" (date 01 02 2004) encrypt decrypt)) - Crea socialnetwork.
;(define face (register face (date 01 04 2021) "rodrigo" "12345")) - Registra usuario.
;(define face (register face (date 12 09 2021) "stefane" "abcde")) - Registra usuario.
;(define face (register face (date 12 09 2021) "martina" "abcde")) - Registra usuario.
;(define face (register face (date 12 09 2021) "bastian" "abcde")) - Registra usuario.

;(define face (((login face "rodrigo" "12345" follow) (date 30 10 2020)) "stefane")) - Sigue a usuario.
;(define face (((login face "rodrigo" "12345" follow) (date 30 10 2020)) "martina")) - Sigue a usuario.
;(define face (((login face "rodrigo" "12345" follow) (date 30 10 2020)) "bastian")) - Sigue a usuario.

;(define face (((login face "rodrigo" "12345" post) (date 30 10 2020)) "primer post")) - Post en muro propio con usuario logeado.
;(define face (((login face "stefane" "abcde" post) (date 30 10 2020)) "segundo post")) - Post en muro propio con usuario logeado.
;(define face (((login face "rodrigo" "12345" post) (date 30 10 2020)) "segundo post" "stefane")) - Post en muro de amigo.
;(define face (((login face "rodrigo" "12345" post) (date 30 10 2020)) "post multiple" "stefane" "martina" "bastian")) - Post en muro de n amigos.


;-------------------------------------------------------------------------------------------------


;FUNCION SHARE

;Descripción: Función que permite a un usuario compartir una publicacion de x usuario en su propio muro, o en el muro de demas usuarios.
;Dominio: socialnetwork
;Recorrido: date x entero x list
;Recorrido socialnetwork

(define share (lambda (sn)
               (lambda (dt)
                 (lambda (postID . users)
                   (let ([loggeduser (get-user (get-account-list sn) (get-logged-user sn))])
                     (let ([pub (search-publication (get-account-list sn) postID)])
                       (if (null? pub)
                           sn
                           (if (null? users)
                               (f-aux1 sn (get-username loggeduser) dt ((get-encrypt sn) (get-publication-content pub)) users (get-publication-id pub))                              
                               (post-to-users sn (only-friend-list (get-account (get-account-list sn) (get-username loggeduser)) users null) dt
                                              ((get-encrypt sn) (get-publication-content pub)) (get-username loggeduser) (get-publication-id pub))                          
                            ) 
                        )
                      )                     
                    ) 
))))


;EJEMPLOS DE PRUEBA PARA SHARE

;(define face (socialnetwork "Facebook" (date 01 02 2004) encrypt decrypt)) - Crea socialnetwork.
;(define face (register face (date 01 04 2021) "rodrigo" "12345")) - Registra usuario.
;(define face (register face (date 12 09 2021) "stefane" "abcde")) - Registra usuario.
;(define face (register face (date 12 09 2021) "martina" "abcde")) - Registra usuario.
;(define face (register face (date 12 09 2021) "bastian" "abcde")) - Registra usuario.

;(define face (((login face "rodrigo" "12345" follow) (date 30 10 2020)) "stefane")) - Sigue a usuario.
;(define face (((login face "rodrigo" "12345" follow) (date 30 10 2020)) "martina")) - Sigue a usuario.
;(define face (((login face "rodrigo" "12345" follow) (date 30 10 2020)) "bastian")) - Sigue a usuario.


;(define face (((login face "rodrigo" "12345" post) (date 30 10 2020)) "primer post")) - Post en muro propio con usuario logeado.
;(define face (((login face "stefane" "abcde" post) (date 30 10 2020)) "segundo post")) - Post en muro propio con usuario logeado.
;(define face (((login face "rodrigo" "12345" post) (date 30 10 2020)) "segundo post" "stefane")) - Post en muro de amigo.
;(define face (((login face "rodrigo" "12345" post) (date 30 10 2020)) "post multiple" "stefane" "martina" "bastian")) - Post en muro de n amigos.

;(define face (((login face "rodrigo" "12345" share) (date 30 10 2020)) 6)) - Se comparte post ID 6 en publicaciones propias.
;(define face (((login face "rodrigo" "12345" share) (date 30 10 2020)) 2)) - Se comparte post ID 2 en publicaciones propias.
;(define face (((login face "rodrigo" "12345" share) (date 30 10 2020)) 2 "bastian" "martina")) - se comparte post ID 2 en publicaciones de amigos.


;-------------------------------------------------------------------------------------------------


;FUNCION SOCIALNETWORK->STRING

;Descripción: Función que imprime una red social de manera legible.
;Dominio: socialnetwork
;Recorrido: string
;Se utiliza recursion de cola en función auxiliar aux-social-string la cual recorre cada cuenta de la socialnetwork.

(define socialnetwork->string (lambda (sn)
       (if (equal? (get-logged-user sn) "")
           (aux-social-string (get-account-list sn) (get-account-list sn) "" (get-decrypt sn))
           (aux-social-string (filter (lambda (x) (equal? (car (car x)) (get-logged-user sn))) (get-account-list sn))
                              (get-account-list sn) "" (get-decrypt sn))
       )
))

(define aux-social-string (lambda (account-list-temp account-list temp-string decryptFn)
       (if (null? account-list-temp)
           temp-string
           (aux-social-string (cdr account-list-temp) account-list
                              (string-append temp-string (account->string (car account-list-temp) account-list decryptFn)) decryptFn)
        )
))

;EJEMPLOS DE PRUEBA PARA SOCIALNETWORK->STRING

;(define face (socialnetwork "Facebook" (date 01 02 2004) encrypt decrypt))
;(define face (register face (date 01 04 2021) "rodrigo" "12345"))
;(define face (register face (date 12 09 2021) "stefane" "abcde"))
;(define face (register face (date 12 09 2021) "martina" "abcde"))
;(define face (register face (date 12 09 2021) "bastian" "abcde"))

;(define face (((login face "rodrigo" "12345" follow) (date 30 10 2020)) "stefane"))
;(define face (((login face "rodrigo" "12345" follow) (date 30 10 2020)) "martina"))
;(define face (((login face "rodrigo" "12345" follow) (date 30 10 2020)) "bastian"))
;(define face (((login face "stefane" "abcde" follow) (date 30 10 2020)) "rodrigo"))
;(define face (((login face "stefane" "abcde" follow) (date 30 10 2020)) "bastian"))
;(define face (((login face "stefane" "abcde" follow) (date 30 10 2020)) "martina"))
;(define face (((login face "bastian" "abcde" follow) (date 30 10 2020)) "rodrigo"))

;(define face (((login face "rodrigo" "12345" post) (date 01 02 2021)) "primer post rodrigo"))
;(define face (((login face "stefane" "abcde" post) (date 04 01 2021)) "primer post stefane"))
;(define face (((login face "stefane" "abcde" post) (date 10 03 2021)) "Hola Rodrigo" "rodrigo"))
;(define face (((login face "bastian" "abcde" post) (date 26 04 2021)) "Hola Rodrigo" "rodrigo"))
;(define face (((login face "bastian" "abcde" post) (date 30 2 2021)) "Hola soy bastian"))
;(define face (((login face "stefane" "abcde" post) (date 01 02 2021)) "Hola martina" "martina"))

;(define face (((login face "rodrigo" "12345" share) (date 02 05 2021)) 4))
;(define face (((login face "stefane" "abcde" share) (date 15 01 2021)) 4 "rodrigo"))
;(define face (((login face "stefane" "abcde" share) (date 15 01 2021)) 1 "martina"))



;(display (socialnetwork->string face))
;(display (login face "rodrigo" "12345" socialnetwork->string))



;-------------------------------------------------------------------------------------------------



;FUNCIONES AUXILIARES PARA POST Y SHARE CON EL FIN DE DEJAR UNA LECTURA MAS LIMPIA EN EL CODIGO.
(define f-aux1 (lambda (sn usr dt content  user-from pOrigenID) 
     (list (get-social-name sn) (get-social-date sn)
           (f-aux2 usr (get-account-list sn) ((get-encrypt sn) content) dt user-from (+ (get-last-pubID sn) 1) pOrigenID) ""
           (get-encrypt sn) (get-decrypt sn) (+ (get-last-pubID sn) 1))
))


(define f-aux2 (lambda (usr account-list body dt user-from pID pOrigenID)
      (if (null? user-from)
          (update-account usr (update-account-publication (get-account account-list usr)
          (add-publication pID (get-account-pub (get-account account-list usr)) usr body dt pOrigenID)) account-list null)
          
          (update-account usr (update-account-publication (get-account account-list usr)
          (add-publication pID (get-account-pub (get-account account-list usr)) user-from body dt pOrigenID)) account-list null)
       )      
))

;FUNCION post-to-users
;Descripción: Función recursiva que permite aplicar un post a un listado n de usuarios.
;Dominio: socialnetwork x list x date x string x string
;Recorrido: funcion
;Se utiliza recursion de cola para evitar desbordamiento al postear en mulitples usuarios.

(define post-to-users (lambda (sn user-to dt content user-from pOrigenID)
       (if (null? user-to)
           sn
           (post-to-users (f-aux1 sn (car user-to) dt content user-from pOrigenID) (cdr user-to) dt content user-from pOrigenID)
       )
 ))
