#lang racket
(require "suffix-tree.rkt")

(provide (all-defined-out))

; Implementați o funcție care primește două cuvinte (liste
; de caractere) w1 și w2 și calculează cel mai lung prefix
; comun al acestora, împreună cu restul celor două cuvinte
; după eliminarea prefixului comun.
; ex:
; (longest-common-prefix '(#\w #\h #\y) '(#\w #\h #\e #\n))
; => '((#\w #\h) (#\y) (#\e #\n))
; Folosiți recursivitate pe coadă.
(define (find-longest-common-prefix w1 w2 common)
  (cond
    [(or (null? w1) (null? w2) (not (char=? (car w1) (car w2))))
     (list (reverse common) w1 w2)
     ]
    [else
     (find-longest-common-prefix (cdr w1) (cdr w2) (cons (car w1) common))
     ]
    )
  )

(define (longest-common-prefix w1 w2)
  (find-longest-common-prefix w1 w2 '())
  )


; Implementați recursiv o funcție care primește o listă nevidă 
; de cuvinte care încep cu același caracter și calculează cel 
; mai lung prefix comun al acestora.
; Opriți căutarea (parcurgerea) în momentul în care aveți garanția 
; că prefixul comun curent este prefixul comun final.
(define (longest-common-prefix-of-list words)
  (define (reduce-prefix words)
    (if (or (null? words) (null? (cdr words)))
        (car words)
        (let* ((prefix-pair (longest-common-prefix (car words) (cadr words)))
               (common-prefix (car prefix-pair)))
          (reduce-prefix (cons common-prefix (cddr words)))
          )
        )
    )

  (reduce-prefix words)
  )



;; Următoarele două funcții sunt utile căutării unui șablon
;; (pattern) într-un text cu ajutorul arborelui de sufixe.
;; Ideea de căutare este următoarea:
;; - dacă șablonul există în text, atunci există un sufix care
;;   începe cu acest șablon, deci există o cale care începe din
;;   rădăcina arborelui care se potrivește cu șablonul
;; - vom căuta ramura a cărei etichetă începe cu prima literă
;;   din șablon
;; - dacă nu găsim această ramură, șablonul nu apare în text
;; - dacă șablonul este conținut integral în eticheta ramurii,
;;   atunci el apare în text
;; - dacă șablonul se potrivește cu eticheta dar nu este conținut
;;   în ea (de exemplu șablonul "nana$" se potrivește cu eticheta
;;   "na"), atunci continuăm căutarea în subarborele ramurii
;; - dacă șablonul nu se potrivește cu eticheta (de exemplu
;;   șablonul "numai" nu se potrivește cu eticheta "na"), atunci
;;   el nu apare în text (altfel, eticheta ar fi fost "n", nu
;;   "na", pentru că eticheta este cel mai lung prefix comun al
;;   sufixelor din subarborele său)



; Implementați funcția match-pattern-with-label care primește un
; arbore de sufixe și un șablon nevid și realizează un singur pas 
; din procesul prezentat mai sus - identifică ramura arborelui a
; cărei etichetă începe cu prima literă din șablon, apoi
; determină cât de bine se potrivește șablonul cu eticheta,
; întorcând ca rezultat:
; - true, dacă șablonul este conținut integral în etichetă
; - lista (etichetă, nou pattern, subarbore), dacă șablonul se
;   potrivește cu eticheta dar nu este conținut în ea
;   (ex: ("na", "na$", subarborele de sub eticheta "na")
;   pentru șablonul inițial "nana$" și eticheta "na")
; - lista (false, cel mai lung prefix comun între etichetă și
;   șablon), dacă șablonul nu s-a potrivit cu eticheta sau nu
;   s-a găsit din start o etichetă care începe cu litera dorită
;   (ex1: (false, "n") pentru șablonul "numai" și eticheta "na")
;   (ex2: (false, "") pentru etichetă negăsită)
; Obs: deși exemplele folosesc stringuri pentru claritate, vă
; reamintim că în realitate lucrăm cu liste de caractere.
(define (common-prefix-length lst1 lst2)
  (define (helper l1 l2 count)
    (if (or (null? l1)
            (null? l2)
            (not (char=? (car l1) (car l2)))) ; not match
        count ; return the count of matching characters
        (helper (cdr l1) (cdr l2) (+ 1 count))  ; continue with the rest of the lists
        )
    )

  (helper lst1 lst2 0)
  )

(define (match-pattern-with-label st pattern)
  (let ((branch (get-ch-branch st (car pattern))))
    (if (not branch) ; check if no branch starts with the pattern's first character
        (list #f '()) ; pattern not found in the text
        (let* ((label (get-branch-label branch))
               (subtree (get-branch-subtree branch))
               (common-prefix-length (common-prefix-length pattern label)))
          (cond
            ((equal? common-prefix-length (length pattern))
             #t) ; fully matches the start of the label
            ((equal? common-prefix-length (length label))
             (if (= common-prefix-length 0)
                 (list #f '()) ; no common prefix
                 (list label (drop pattern common-prefix-length) subtree))) ; partial match, need to search deeper
            (else
             (list #f (take pattern common-prefix-length)) ; partial or no match, return common prefix
             )
            )
          )
        )
    )
  )



; Implementați funcția st-has-pattern? care primește un
; arbore de sufixe și un șablon și întoarce true dacă șablonul
; apare în arbore, respectiv false în caz contrar.
(define (st-has-pattern? st pattern)
  (define (search-in-subtree subtree pattern)
    (cond
      [(null? pattern) #t] ; if pattern is empty, it's found
      [(null? subtree) #f] ; if subtree is empty, pattern not found
      [else
       (let ((match-result (match-pattern-with-label subtree pattern)))
         (cond
           [(eq? match-result #t) #t] ; fully matched within a label
           [(and (list? match-result) (eq? (first match-result) #f)) #f] ; does not match label
           [(list? match-result) 
            (search-in-subtree (third match-result) (second match-result))] ; partial match, continue in the subtree
           [else #f]
           )
         )
       ]
      )
    )
  (search-in-subtree st pattern)
  )
