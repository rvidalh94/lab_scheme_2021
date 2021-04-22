#lang racket

(provide date)
(provide is-date)

;TDA Date

;CONSTRUCTOR

;Función date
;Descripcion: Función que crea un date con el siguiente formato (dd mm yyyy)
;Dominio: entero x entero x entero x entero x entero.
;Recorrido: lista con la fecha en el siguiente formato (dd mm yyyy HH:mm)

(define date (lambda (dd mm yyyy hh MM)
  (list dd mm yyyy hh MM)
))


;PERTENENCIA

;Funcion isdate
;Descripcion: Función que valida si el argumento dado pertenece a la definicion de date
;Dominio: Dato
;Recorrido: True o False dependiendo de las validaciones

(define is-date (lambda (paramdate)
  (if (list? paramdate)
      (if (= (length paramdate) 3)
          (if (and (integer? (car paramdate)) (integer? (car (cdr paramdate))) (integer? (car (cdr (cdr paramdate))))
                   (integer? (car (cdr (cdr (cdr paramdate))))) (integer? (car (cdr (cdr (cdr (cdr paramdate)))))))
              #t
              #f
           )#f
       )#f
 )))


;SELECTORES

;Función ge-tday
;Descripcion: Función que retorna el día del date
;Dominio: date
;Recorrido: entero

(define get-day (lambda (date)
  (car date)
))

;Función get-month
;Descripcion: Función que retorna el mes del date
;Dominio: date
;Recorrido: entero

(define get-month (lambda (date)
  (car (cdr date))
))


;Función get-year

;Función que retorna el año del date
;Dominio: date
;Recorrido: entero

(define get-year (lambda (date)
  (car (cdr (cdr date)))
))


;MODIFICADORES

;Función mod-day
;Descripción: Función que modifica el día de un date
;Dominio: date x entero
;Recorrido: date

(define mod-day (lambda (date newday)
  (list newday (get-month date) (get-year date))
))


;Función mod-month
;Descripción: Función que modifica el mes de un date
;Dominio: date x entero
;Recorrido: date

(define mod-month (lambda (date newmonth)
  (list (get-day date) newmonth (get-year date))
))


;Función mod-year
;Descripción: Función que modifica el año de un date
;Dominio: date x entero
;Recorrido: date

(define mod-year (lambda (date newyear)
  (list (get-day date) (get-month date) newyear)
))
