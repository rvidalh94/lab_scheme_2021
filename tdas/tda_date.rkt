#lang racket

;TDA DATE


;CONSTRUCTOR

;date
;Descripcion: Función que crea un date con el siguiente formato (dd mm yyyy).
;Dominio: entero x entero x entero
;Recorrido: lista

(define date (lambda (dd mm yyyy)
    (if (and (integer? dd) (integer? mm) (integer? yyyy))
        (list dd mm yyyy)
        null
     )
))



;PERTENENCIA

;date?
;Descripcion: Función que valida si el argumento dado pertenece a la definicion de date.
;Dominio: valor
;Recorrido: booleano

(define date? (lambda (paramdate)
  (if (list? paramdate)
      (if (= (length paramdate) 3)
          (if (and (integer? (car paramdate)) (integer? (car (cdr paramdate))) (integer? (car (cdr (cdr paramdate)))))
              #t
              #f
           )#f
       )#f
 )))


;SELECTORES

;get-day
;Descripcion: Función que retorna el día del date.
;Dominio: date
;Recorrido: entero

(define get-day (lambda (date)
  (car date)
))


;get-month
;Descripcion: Función que retorna el mes del date.
;Dominio: date
;Recorrido: entero

(define get-month (lambda (date)
  (car (cdr date))
))


;get-year
;Función que retorna el año del date.
;Dominio: date
;Recorrido: entero

(define get-year (lambda (date)
  (car (cdr (cdr date)))
))


;MODIFICADORES

;mod-day
;Descripción: Función que modifica el día de un date.
;Dominio: date x entero
;Recorrido: date

(define mod-day (lambda (date newday)
  (list newday (get-month date) (get-year date))
))


;mod-month
;Descripción: Función que modifica el mes de un date.
;Dominio: date x entero
;Recorrido: date

(define mod-month (lambda (date newmonth)
  (list (get-day date) newmonth (get-year date))
))


;mod-year
;Descripción: Función que modifica el año de un date.
;Dominio: date x entero
;Recorrido: date

(define mod-year (lambda (date newyear)
  (list (get-day date) (get-month date) newyear)
))


(provide date)
(provide date?)
