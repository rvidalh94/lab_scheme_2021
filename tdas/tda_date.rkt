#lang racket

;TDA Date

;CONSTRUCTOR

;Función date
;Descripcion: Función que crea un date con el siguiente formato (dd mm yyyy)
;Dominio: entero, entero, entero.
;Recorrido: lista con la fecha en el siguiente formato (dd mm yyyy)

(define (date dd mm yyyy)
  (list dd mm yyyy)
)


;PERTENENCIA

;Funcion isdate
;Descripcion: Función que valida si el argumento dado pertenece a la definicion de date
;Dominio: argumento pasado a la función, puede ser cualquier valor o tipo de dato.
;Recorrido: True o False dependiendo de las validaciones

(define (isdate paramdate)
  (if (list? paramdate)
      (if (= (length paramdate) 3)
          (if (and (integer? (car paramdate)) (integer? (car (cdr paramdate))) (integer? (car (cdr (cdr paramdate)))))
              #t
              #f
           )#f
       )#f
 ))


;SELECTORES

;Función getday
;Descripcion: Función que retorna el día del date
;Dominio: date
;Recorrido: entero

(define (getday date)
  (car date)
)

;Función getmonth
;Descripcion: Función que retorna el mes del date
;Dominio: date
;Recorrido: entero

(define (getmonth date)
  (car (cdr date))
)


;Función getyear

;Función que retorna el año del date
;Dominio: date
;Recorrido: entero

(define (getyear date)
  (car (cdr (cdr date)))
)


;MODIFICADORES

;Función modday
;Descripción: Función que modifica el día de un date
;Dominio: date, entero
;Recorrido: date

(define (modday date newday)
  (list newday (car (cdr date)) (car (cdr (cdr date))))
)


;Función modmonth
;Descripción: Función que modifica el mes de un date
;Dominio: date, entero
;Recorrido: date

(define (modmonth date newmonth)
  (list (car date) newmonth (car (cdr (cdr date))))
)


;Función modyear
;Descripción: Función que modifica el año de un date
;Dominio: date, entero
;Recorrido: date

(define (modyear date newyear)
  (list (car date) (car (cdr date)) newyear)
)