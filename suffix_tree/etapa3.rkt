#lang racket
(require "suffix-tree.rkt")
(require "etapa1.rkt")
(require "etapa2.rkt")

(provide (all-defined-out))

;; Această etapă este dedicată aplicațiilor 
;; arborelui de sufixe:
;; - găsirea unui șablon într-un text
;; - cel mai lung subșir comun a două texte
;; - găsirea unui subșir de lungime dată care se
;;   repetă în text
;; Conform convenției din etapele anterioare, un text
;; este întotdeauna reprezentat ca listă de caractere.
;; Rezultatele funcțiilor de mai jos sunt de asemenea
;; reprezentate ca liste de caractere.


; Implementați funcția substring? care primește un text și
; un șablon nevid și întoarce true dacă șablonul apare în 
; text, respectiv false în caz contrar.
; Observație: ați implementat deja logica principală a
; acestei căutări în etapa 1, în funcția st-has-pattern?,
; care este un operator al tipului ST. Acum aveți toate
; uneltele necesare implementării operatorului corespunzător
; pentru tipul text (pentru că în etapa 2 ați implementat
; construcția arborelui de sufixe asociat unui text).
; (define (substring? text pattern)
;   'your-code-here)
(define (substring? text pattern)
  (let* ((text-with-end-marker (append text '(#\$)))  ; text ends with a unique marker
         (suffix-tree (text->cst text-with-end-marker)))  ; construct the compact suffix tree for the text
    (st-has-pattern? suffix-tree pattern)))  ; use st-has-pattern? to check if the pattern exists in the suffix tree



; TODO 2
; Implementați funcția longest-common-substring care primește
; două texte și determină cel mai lung subșir comun al
; acestora, folosind algoritmul următor:
; 1. Construiește arborele de sufixe ST1 pentru primul text.
; 2. Pentru fiecare sufix din al doilea text (de la cel mai
;    lung la cel mai scurt), găsește cea mai lungă potrivire 
;    cu sufixele din primul text, urmând căile relevante în ST1.
; 3. Rezultatul final este cel mai lung rezultat identificat
;    la pasul 2 (în caz de egalitate a lungimii, păstrăm
;    primul șir găsit).
; Folosiți named let pentru a parcurge sufixele.
; Observație: pentru sufixele din al doilea text nu dorim 
; marcajul de final $ pentru a nu crește artificial lungimea 
; șirului comun cu acest caracter.
; Hint: Revizitați funcția match-pattern-with-label (etapa 1).

(define (longest-common-substring text1 text2)
  (let* ((st1 (text->cst (string->list (string-append (list->string text1) "$"))))  ; Construiește CST pentru text1 cu marcator de sfârșit.
         (suffixes (sort (get-suffixes (string->list (list->string text2)))  ; Generează și sortează sufixele text2.
                         (lambda (a b) (> (length a) (length b))))))  ; Sortare descrescătoare după lungime.
    (let loop ((suffixes suffixes) (longest '()))  ; Named let pentru iterare.
      (if (null? suffixes)
          longest  ; Returnează cel mai lung subșir comun găsit.
          (let* ((suffix (car suffixes))
                 (match (find-match st1 suffix '())))  ; Caută potrivirea pentru sufix în CST.
            (if (> (length match) (length longest))
                (loop (cdr suffixes) match)  ; Actualizează cel mai lung subșir comun dacă este cazul.
                (loop (cdr suffixes) longest)))))))  ; Continuă căutarea cu următorul sufix.


(define (find-match st suffix accumulated)
  (if (null? suffix)  ; Dacă sufixul curent a fost complet consumat.
      accumulated
      (let* ((branch (get-ch-branch st (car suffix))))  ; Caută ramura care începe cu primul caracter al sufixului.
        (if branch  ; Dacă există o astfel de ramură
            (let* ((label (get-branch-label branch))  ; Obține eticheta ramurii
                   (subtree (get-branch-subtree branch))  ; Obține subarborele de sub ramura respectivă
                   (match-length (common-prefix-length suffix label)))  ; Calculează lungimea prefixului comun
              (if (= match-length 0)  ; Dacă nu există un prefix comun
                  (reverse accumulated)  ; Returnează ceea ce s-a acumulat până acum
                  (find-match subtree  ; Continuă căutarea în subarbore
                               (drop suffix match-length)  ; Elimină prefixul comun din sufix
                               (append accumulated (take label match-length)))))  ; Adaugă prefixul comun la acumulat
            accumulated))))  ; Dacă nu există ramură, returnează acumulatul



; TODO 3
; Implementați funcția repeated-substring-of-given-length
; care primește un text și un număr natural len și
; parcurge arborele de sufixe al textului până găsește un
; subșir de lungime len care se repetă în text.
; Dacă acest subșir nu există, funcția întoarce false.
; Obs: din felul în care este construit arborele de sufixe
; (pe baza alfabetului sortat), rezultatul va fi primul 
; asemenea subșir din punct de vedere alfabetic.
; Ideea este următoarea: orice cale în arborele de sufixe
; compact care se termină cu un nod intern (un nod care 
; are copii, nu este o frunză) reprezintă un subșir care
; se repetă, pentru că orice asemenea cale reprezintă un
; prefix comun pentru două sau mai multe sufixe ale textului.
; Folosiți interfața definită în fișierul suffix-tree
; atunci când manipulați arborele.
(define (repeated-substring-of-given-length text len)
  'your-code-here)
