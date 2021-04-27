#lang racket

(require "tda_date.rkt")

;TDA de user

;CONSTRUCTOR

;Función user
;Descripcion: Función que permite la creación de un usuario.
;Dominio: string x string x date
;Recorrido: lista

(define user (lambda (username password dt)
    (if (and (string? username) (string? password) (date? dt))
        (list username password dt)
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
      (if (= (length paramuser) 3)
          (if (and (string? (car paramuser)) (string? (car (cdr paramuser))) (date? (car (cdr (cdr paramuser)))))
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


;Otras Funciones

;get-user
;Validad si existe usuario en una lista de usuarios
;Dominio: lista x user
;Recorrido: booleano

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



(provide user)
(provide get-user)
(provide get-username)
(provide get-password)
(provide add-user)
