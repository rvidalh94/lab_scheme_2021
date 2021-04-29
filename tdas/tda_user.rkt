#lang racket

(require "tda_date.rkt")

;TDA de user

;CONSTRUCTOR

;Función user
;Descripcion: Función que permite la creación de un usuario.
;Dominio: string x string x date x lista
;Recorrido: lista

(define user (lambda (username password dt)
    (if (and (string? username) (string? password) (date? dt))
        (list username password dt null)
         null
     )
))

;PERTENTENCIA

;Función user?
;Descripcion: Función que permite verificar si el argumento dado a la función pertenece a un user
;Dominio: Dato
;Recorrido: booleano

(define user? (lambda (paramuser)
  (if (list? paramuser)
      (if (= (length paramuser) 4)
          (if (and (string? (car paramuser)) (string? (car (cdr paramuser))) (date? (car (cdr (cdr paramuser)))) (list? (car (cdr (cdr (cdr paramuser))))))
              #t
              #f
           )#f
       )#f
 )))


;SELECTORES

;Función get-username
;Descripcion: retorna el username del user
;Dominio: user
;Recorrido: string

(define get-username (lambda (user)
  (car user)
))

;Función get-password
;Descripcion: retorna el password del user
;Dominio: user
;Recorrido: string
(define get-password (lambda (user)
  (car (cdr user))
))

;Función get-user-date
;Descripcion: retorna el password del user
;Dominio: user
;Recorrido: string
(define get-user-date (lambda (user)
  (car (cdr (cdr user)))
))

;Función get-user-friends
;Descripcion: retorna el password del user
;Dominio: user
;Recorrido: string
(define get-user-friends (lambda (user)
  (car (cdr (cdr (cdr user))))
))



;MODIFICADORES

;Función changeusername
;Descripcion: Funcion para modificar el username del user. Recibe al user y el nuevo username, retorna al user modificado.
;Dominio: user x string.
;Recorrido: user

(define change-username (lambda (user newusername)
  (list newusername (get-password user) (get-user-date user))
 ))

;Función changeuserpass
;Descripcion: Funcion para modificar el password del user. Recibe al user y la nueva password, retorna al user modificado.
;Dominio: user x string.
;Recorrido: user

(define change-userpass (lambda (user newpass)
  (list (get-username user) newpass (get-user-date user))
 ))


;Función update-user-friends
;Descripcion: Funcion para modificar el password del user. Recibe al user y la nueva password, retorna al user modificado.
;Dominio: user x lista.
;Recorrido: user

(define update-user-friends (lambda (user nl)
  (list (get-username user) (get-password user) (get-user-date user) nl)
 ))


;Otras Funciones

;get-user
;obtiene un usuario en una lista de usuarios
;Dominio: lista x string
;Recorrido: user

(define get-user (lambda (userlist b)
     (if (null? userlist)
         null
         (if (equal? (get-username (car userlist)) b)
             (car userlist)
             (get-user (cdr userlist) b)
          )
      )
))

; (get-user (list (user "andres" "1234" (date 01 01 2000)) (user "basti" "1234" (date 01 01 2000)) (user "stefane" "1234" (date 01 01 2000)) (user "rodrigo" "1234" (date 01 01 2000))) "rodrigo")


;add-user
;Funcion para agregar a un user al final de la lista de usuarios
;Dominio: list x user
;Recorrido: lista
(define add-user(lambda (userlist b)
    (if (empty? userlist)
        (cons b null)
        (if (eqv? (car userlist) null)
            (cons b null)
            (cons (car userlist) (add-user (cdr userlist) b))))))

;add-friend
;Funcion para agregar id de amigo al final de la lista de friends
;Dominio: list x string
;Recorrido: lista
(define add-friend(lambda (usr fid)
    (if (empty? (get-user-friends usr))
        (cons fid null)
        (if (eqv? (car (get-user-friends usr)) null)
            (cons fid null)
            (cons (car (get-user-friends usr)) (add-user (cdr (get-user-friends usr)) fid))))))

;(define user1 (user "rodrigo" "12345" (date 27 04 2021)))
;(define user2 (update-user-friends user1 (add-friend user1 "stefane")))


;exist-friend
;Validad si existe username en una lista de amigos
;Dominio: lista x string
;Recorrido: booleano

(define exist-friend (lambda (friend-list friend-name)
     (if (null? friend-list)
         #f
         (if (equal? (car friend-list) friend-name)
             #t
             (exist-friend (cdr friend-list) friend-name)
          )
      )
))


;update-user-in-list
;actualiza a un usuario dentro de una lista de usuarios
;Dominio: user x user x lista
;Recorrido: lista

(define (update-user-in-list userOld userNew userlist templist)
 (cond
  ((null? userlist) '())
  ((eq? (get-username (car userlist)) (get-username userOld))
    (if (null? templist)
        (cons userNew (cdr userlist))
        (cons templist (cons userNew (cdr userlist)))
    ))
  (else
    (if (null? templist)
        (update-user-in-list userOld userNew (cdr userlist) (car userlist))
        (update-user-in-list userOld userNew (cdr userlist) (cons templist (car userlist)))
    )
)))



;(define (update-user-in-list userOld userNew userlist)
 ;(cond
  ;((null? userlist) '())
  ;((list? (car userlist)) (cons (update-user-in-list userOld userNew (car userlist)) (update-user-in-list userOld userNew (cdr userlist))))
  ;((eq? (car userlist) (get-username userOld)) userNew)
  ;(else
  ; (cons (car userlist) (update-user-in-list userOld userNew (cdr userlist))))))


(provide user)
(provide get-user)
(provide get-username)
(provide get-password)
(provide add-user)
(provide update-user-friends)
(provide add-friend)
(provide exist-friend)
(provide update-user-friends)
(provide get-user-friends)
(provide update-user-in-list)

;(define lista1 (list (user "rodrigo" "1234" (date 01 01 2020)) (user "stefane" "1234" (date 01 01 2020)) (user "basti" "1234" (date 01 01 2020))))
;(define old (car lista1))
;(define new (list "rodrigo" "1234" (list 1 1 2020) (list "stefane")))
;(update-user-in-list old new lista1 null)

