#lang racket

;TDA de user

;CONSTRUCTOR

;Función user
;Descripcion: Función que permite la creación de un usuario.
;Dominio: string x string
;Recorrido: Lista con el user creado

(define user (lambda (username password)
    (if (and (string? username) (string? password))
        (list username password)
         null
     )
))

;PERTENTENCIA

;Función user?
;Descripcion: Función que permite verificar si el argumento dado a la función pertenece a un user
;Dominio: Dato
;Recorrido: True o False, dependiendo de la validación aplicada.

(define user? (lambda (paramuser)
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


;Otras Funciones

;user-exist
;Validad si existe usuario en una lista de usuarios
;Dominio: lista x user
;Recorrido: true o false dependiendo la validacion

(define user-exist (lambda (userlist b)
     (if (null? userlist)
         #f
         (if (equal? (get-username (car userlist)) b)
             #t
             (user-exist (cdr userlist) b)
          )
      )
))

; (user-exist (list (user "andres" "1234") (user "basti" "1234") (user "stefane" "1234") (user "rodrigo" "1234")) "rodrigo")


;add-user
;Funcion para agregar a un user al final de la lista de usuarios
;Dominio: list x user
(define add-user(lambda (userlist b)
    (if (empty? userlist)
        (cons b null)
        (if (eqv? (car userlist) null)
            (cons b null)
            (cons (car userlist) (add-user (cdr userlist) b))))))


(provide user)
(provide user-exist)
