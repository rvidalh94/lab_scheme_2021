#lang racket


;append-propio
;Descripción: Función que agrega a un elemento al final de la lista.
;Dominio: lista x elemento
;Recorrido: lista
;utiliza recursion natural.

(define appendPropio
  (lambda (parActual nuevoElemento)
    (if (empty? parActual)
        (cons nuevoElemento null)
        (if (eqv? (car parActual) null)
            (cons nuevoElemento null)
            (cons (car parActual) (appendPropio (cdr parActual) nuevoElemento))))))


;Funcion encrypt
;Descripción: Función que encripta un texto inviertiendo su contenido.
;Dominio: string
;Recorrido: string
(define encrypt (lambda (content)
     (list->string (reverse (string->list content)))
))


;Funcion decrypt
;Descripción: Función que desencripta un texto inviertiendo su contenido.
;Dominio: string
;Recorrido: string
(define decrypt (lambda (content)
     (list->string (reverse (string->list content)))
))


(provide appendPropio)
(provide encrypt)
(provide decrypt)