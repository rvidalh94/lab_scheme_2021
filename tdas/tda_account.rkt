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
;Recorrido: account
(define account (lambda(usr dt)
    (list usr dt  null  null null)
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
;Recorrido: date
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
;Descripción: Función para una cuenta nueva al final de la lista de cuentas.
;Dominio: list x user x date
;Recorrido: lista
;Se utiliza recursión natural, ya que se agrega solo de a 1 elemento a la vez,
;por lo que no deberiamos tener un eventual desbordamiento.

(define add-account(lambda (account-list usr dt)
    (if (empty? account-list)
        (cons (account usr dt) null)
        (if (eqv? (car account-list) null)
            (cons (account usr dt) null)
            (cons (car account-list) (add-account (cdr account-list) usr dt))))))


;get-user
;Descripción: Función que obtiene un usuario en una lista de cuentas.
;Dominio: lista x string
;Recorrido: user
;Se utiliza recursión de cola, ya que se debe buscar en una extensa de cuentas.

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
;Se utiliza recursión de cola, ya que al buscar una cuenta en n cuentas, si el n es muy grande, puede provocar desbordamiento.

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



;update-account
;Descripción: Función que una cuenta dentro de una lista de cuentas.
;Dominio: string x account x lista x lista
;Recorrido: lista
;Se utiliza recursión de cola, ya que al momento de buscar una cuenta para su actualizar,
;la busqueda se puede tornar muy exaustiva al muchas cuentas.


(define update-account (lambda (username updated-account account-list templist)
   (if(eq? (get-username (get-account-user (car account-list))) username)
       (if (null? templist)
            (cons updated-account (cdr account-list))
            (append (appendPropio templist updated-account) (cdr account-list))
        )
        (if (null? templist)
            (update-account username updated-account (cdr account-list) (appendPropio templist (car account-list)))
            (update-account username updated-account (cdr account-list) (appendPropio templist (car account-list)))
         )
     )
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


;only-friend-list
;Descripción: Función que recibe una lista de usuarios y solo retorna los contenidos dentro de otra lista.
;Dominio: account x lista x lista
;Recorrido: lista
;Se utiliza recursión de cola.

(define only-friend-list (lambda (account friend-list-temp friend-list-last)
    (if (null? friend-list-temp)
        friend-list-last
        (let ([acnt-friends (get-account-friends account)])
          (cond
            ((member (car friend-list-temp) acnt-friends) (only-friend-list account (cdr friend-list-temp)
                                                                         (appendPropio friend-list-last (car friend-list-temp))))
            (else
             (member (car friend-list-temp) acnt-friends) (only-friend-list account (cdr friend-list-temp) friend-list-last)
             ))
          )
 )))



;search-publication
;Descripción: Función que retorna una publicacion segun un ID dado.
;Dominio: lista
;Recorrido: publicacion
(define search-publication (lambda (account-list pID)
        (if (null? account-list)
            null
            (let ([pub (get-publication (get-account-pub (car account-list)) pID)])
              (if (null? pub)
                  (search-publication (cdr account-list) pID)
                  pub
                  )
             )
         )
))


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
(provide only-friend-list)
(provide search-publication)