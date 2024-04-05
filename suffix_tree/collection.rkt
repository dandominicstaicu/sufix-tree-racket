#lang racket

(provide (all-defined-out))

;; În acest fișier vă definiți constructorii și
;; operatorii tipului Collection.
;; În etapele anterioare, colecțiile erau de fapt
;; liste.
;; În definițiile de mai jos, veți considera că
;; o colecție este implementată ca flux.

; Întrucât stream-cons nu este o funcție obișnuită, 
; ci este definită ca o sintaxă specială, astfel
; încât ea să nu își evalueze argumentele înainte 
; de apel (comportament pe care ni-l dorim și pentru 
; collection-cons), nu putem folosi o definiție
; de tipul
;    (define collection-cons stream-cons)
; (genul acesta de definiție generează o eroare).
; Nici varianta
;    (define (collection-cons x xs) (stream-cons x xs))
; nu este o soluție, întrucât funcțiile definite de noi
; în Racket sunt funcții stricte, iar x și xs vor fi
; evaluate înainte de a intra în corpul funcției
; collection-cons și a descoperi că ele vor fi
; argumentele unui stream-cons.
; Modul de a defini collection-cons pentru a reproduce
; întocmai comportamentul lui stream-cons este:
(define-syntax-rule (collection-cons x xs) (stream-cons x xs))
; Obs: puteți schimba numele funcției, dacă nu vă
; place "collection-cons". Este o funcție folosită doar
; de voi în fișierul etapa4.rkt, nu de checker.


; TODO
; Scrieți în continuare restul definițiilor
; (care nu necesită o sintaxă specială).

;; Retrieves the first element of the collection
(define (collection-first col)
  (stream-first col))

;; Gets the rest of the collection, excluding the first element
(define (collection-rest col)
  (stream-rest col))

;; Checks if a collection is empty
(define (collection-empty? col)
  (stream-empty? col))

;; Applies the function `f` to each element of the collection, returning a new collection
(define (collection-map f col)
  (stream-map f col))

;; Filters the collection `col` using the predicate `pred`, returning a new collection
(define (collection-filter pred col)
  (stream-filter pred col))

(define (empty-collection)
  empty-stream)

(define (collection-drop col n)
  (if (or (collection-empty? col) (= n 0))
      col
      (collection-drop (collection-rest col) (- n 1))
      )
  )

(define (list->collection lst)
  (define (convert-list-to-stream lst)
    (if (null? lst)
        empty-stream  ; If the list is empty, return an empty stream
        (stream-cons (car lst) (convert-list-to-stream (cdr lst)))))  ; Recursively construct the stream
  (convert-list-to-stream lst))

 ; Converts the given collection or stream to a list to work with list operations like append.
  (define (collection->list col)
    (if (collection-empty? col)
        '()
        (cons (collection-first col) (collection->list (collection-rest col)))))


; Helper function to check if a collection is not empty
(define (not-empty? coll)
  (not (collection-empty? coll)))
