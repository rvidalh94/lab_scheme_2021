#lang racket

;TDA de usuario

;CONSTRUCTOR

;Función user
;Descripcion: Función que permite la creación de un usuario.
;Dominio: string (username), string(password)
;Recorrido: Lista con el user creado

(define (user username password)
    (list username password)
)

;PERTENTENCIA

;Función isuser
;Descripcion: Función que permite verificar si el argumento dado a la función pertenece a un user
;Dominio: argumento pasado a la función, puede ser cualquier valor o tipo de dato.
;Recorrido: True o False, dependiendo de la validación aplicada.

(define (isuser paramuser)
  (if (list? paramuser)
      (if (= (length paramuser) 2)
          (if (and (string? (car paramuser)) (string? (car (cdr paramuser))))
              #t
              #f
           )#f
       )#f
 ))


;SELECTORES

;Función getusername
;Descripcion: retorna el username del user
;Dominio: user
;Recorrido:string con la username del user

(define (getusername user)
  (car user)
)

;Función getpassword
;Descripcion: retorna el password del user
;Dominio: user
;Recorrido: string con password del user
(define (getpassword user)
  (car (cdr user))
)


;MODIFICADORES

;Función changeusername
;Descripcion: Funcion para modificar el username del user. Recibe al user y el nuevo username, retorna al user modificado.
;Dominio: user, string.
;Recorrido: user

(define (changeusername user newusername)
  (list newusername (car(cdr user)))
 )

;Función changeuserpass
;Descripcion: Funcion para modificar el password del user. Recibe al user y la nueva password, retorna al user modificado.
;Dominio: user, string.
;Recorrido: user

(define (changeuserpass user newpass)
  (list (car user) newpass)
 )


