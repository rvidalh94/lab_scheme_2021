#lang racket

;TDA de user

;CONSTRUCTOR

;Función user
;Descripcion: Función que permite la creación de un usuario.
;Dominio: string x string
;Recorrido: Lista con el user creado

(define user (lambda (username password)
    (list username password)))

;PERTENTENCIA

;Función is-user
;Descripcion: Función que permite verificar si el argumento dado a la función pertenece a un user
;Dominio: Dato
;Recorrido: True o False, dependiendo de la validación aplicada.

(define is-user (lambda (paramuser)
  (if (list? paramuser)
      (if (= (length paramuser) 2)
          (if (and (string? (car paramuser)) (string? (car (cdr paramuser))))
              #t
              #f
           )#f
       )#f
 )))


;SELECTORES

;Función getusername
;Descripcion: retorna el username del user
;Dominio: user
;Recorrido:string con la username del user

(define get-username (lambda (user)
  (car user)
))

;Función getpassword
;Descripcion: retorna el password del user
;Dominio: user
;Recorrido: string con password del user
(define get-password (lambda (user)
  (car (cdr user))
))


;MODIFICADORES

;Función changeusername
;Descripcion: Funcion para modificar el username del user. Recibe al user y el nuevo username, retorna al user modificado.
;Dominio: user x string.
;Recorrido: user

(define change-username (lambda (user newusername)
  (list newusername (get-password user))
 ))

;Función changeuserpass
;Descripcion: Funcion para modificar el password del user. Recibe al user y la nueva password, retorna al user modificado.
;Dominio: user x string.
;Recorrido: user

(define change-userpass (lambda (user newpass)
  (list (get-username user) newpass)
 ))

(provide user)


