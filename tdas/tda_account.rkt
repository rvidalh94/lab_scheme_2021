#lang racket
(require "tda_user.rkt")
(require "tda_date.rkt")
(require "tda_publication.rkt")
(require "otras_funciones.rkt")

;TDA ACCOUNT

;Constructor

;account
;Descripción: Función que permite la reacion de una cuenta
;Dominio: user x date
(define account (lambda(usr dt)
    (list usr dt  null (cons 0 null) (cons 0 null))
))


;SELECTORES


;Función get-account-user
;Descripción: Función que retorna al user de la cuenta
;Dominio: account
;Recorrido: user
(define get-account-user (lambda (acnt)
  (car acnt)
))

;Función get-account-date
;Descripción: Función que retorna la fecha de creacion de la cuenta.
;Dominio: account
;Recorrido: lista
(define get-account-date (lambda (acnt)
  (car (cdr acnt))
))

;Función get-account-friends
;Descripción: Función que retorna la lista de amigos de la cuenta.
;Dominio: account
;Recorrido: lista
(define get-account-friends (lambda (acnt)
  (car (cdr (cdr acnt)))
))

;Función get-account-pub
;Descripción: Función que retorna la lista publicaciones de la cuenta.
;Dominio: account
;Recorrido: lista
(define get-account-pub (lambda (acnt)
  (car (cdr (cdr (cdr acnt))))
))

;Función get-account-react
;Descripción: Función que retorna la lista reacciones de la cuenta.
;Dominio: account
;Recorrido: lista
(define get-account-react (lambda (acnt)
  (car (cdr (cdr (cdr (cdr acnt)))))
))



;MODIFICADORES

;Función update-account-friends
;Descripción: Función que modifica la lista de amigos de la cuenta.
;Dominio: account x lista.
;Recorrido: account

(define update-account-friends (lambda (acnt nl)
  (list (get-account-user acnt) (get-account-date acnt) nl (get-account-pub acnt) (get-account-react acnt))
 ))


;Función update-account-publication
;Descripción: Función que modifica la lista de publicaciones.
;Dominio: account x lista.
;Recorrido: account

(define update-account-publication (lambda (acnt nl)
  (list (get-account-user acnt) (get-account-date acnt) (get-account-friends acnt) nl (get-account-react acnt))
 ))


;OTRAS FUNCIONES

;add-account
;Descripción: Función para agregar a un user al final de la lista de usuarios.
;Dominio: list x user
;Recorrido: lista
;Se utiliza recursión natural, ya que se agrega solo de a 1 elemento a la vez,
;por lo que no deberiamos tener un eventual desbordamiento.

(define add-account(lambda (account-list b dt)
    (if (empty? account-list)
        (cons (account b dt) null)
        (if (eqv? (car account-list) null)
            (cons (account b dt) null)
            (cons (car account-list) (add-account (cdr account-list) b dt))))))


;get-account
;Descripción: Función que obtiene un usuario en una lista de usuarios mediante el username.
;Dominio: lista x string
;Recorrido: user

(define get-user (lambda (account-list b)
     (if (null? account-list)
         null
         (if (equal? (get-username (get-account-user (car account-list))) b)
             (car (get-account-user account-list))
             (get-user (cdr account-list) b)
          )
      )
))


;get-account
;Descripción: Función que obtiene una cuenta mediante el username
;Dominio: lista x string
;Recorrido: account

(define get-account (lambda (account-list b)
     (if (null? account-list)
         null
         (if (equal? (get-username (get-account-user (car account-list))) b)
             (car account-list)
             (get-account (cdr account-list) b)
          )
      )
))



;add-friend
;Descripción: Función para agregar el username(identificador unico) de amigo al final de la lista de amigos del user.
;Dominio: user x string
;Recorrido: lista
;Se utiliza recursión natural, ya que se agrega solo de a 1 elemento a la vez,
;por lo que no deberiamos tener un eventual desbordamiento.

(define add-friend(lambda (acnt friend-username)
    (if (empty? (get-account-friends acnt))
        (update-account-friends acnt (appendPropio (get-account-friends acnt) friend-username))
        (if (eqv? (car (get-account-friends acnt)) null)
            (update-account-friends acnt (appendPropio (get-account-friends acnt) friend-username))
            (cons (car (get-account-friends acnt)) (add-friend (cdr (get-account-friends acnt)) friend-username))))))



;update-user
;Descripción: Función que actualiza a un usuario dentro de una lista de usuarios
;Dominio: user x user x lista x lista
;Recorrido: lista
;Se utiliza recursión de cola, ya que al momento de buscar un usuario para su actualizar,
;la busqueda se puede tornar muy exaustiva al tener muchos usuarios.


(define update-account (lambda (username updatet-account account-list templist)
   (cond
     ((null? account-list) '())
     ((eq? (get-username (get-account-user (car account-list))) username)
       (if (null? templist)
            (cons updatet-account (cdr account-list))
            (cons templist (cons updatet-account (cdr account-list)))
        ))
      (else
        (if (null? templist)
            (update-account username updatet-account (cdr account-list) (car account-list))
            (update-account username updatet-account (cdr account-list) (cons templist (car account-list)))
         )
   ))
))


;exist-friend
;Descripción: Función que valida si existe o no el username de un user en la lista de amigos de otro user.
;Dominio: lista x string
;Recorrido: booleano
;Se utiliza recursión de cola, ya que si un usuario llega a tener una cantidad significativa de amigos,
;el hecho tan simple de validar si existe, puede provocar un desborde.

(define exist-friend (lambda (friend-list friend-name)
     (if (null? friend-list)
         #f
         (if (equal? (car friend-list) friend-name)
             #t
             (exist-friend (cdr friend-list) friend-name)
          )
      )
))


; POR COMENTAR
(define check-friends (lambda (account friend-list-temp)
    (if (null? friend-list-temp)
        #t
        (let ([acnt-friends (get-account-friends account)])
          (cond
            ((member (car friend-list-temp) acnt-friends) (check-friends account (cdr friend-list-temp)))
            (else
             #f
             ))
          )
    )))


(provide get-account-user)
(provide get-account-date)
(provide get-account-friends)
(provide get-account-pub)
(provide get-account-react)
(provide add-account)
(provide get-user)
(provide update-account)
(provide update-account-friends)
(provide update-account-publication)
(provide exist-friend)
(provide get-account)
(provide check-friends)

