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
(define (substring? text pattern)
  (let* ((text-with-end-marker (append text '(#\$)))  ; text ends with a unique marker
         (suffix-tree (text->cst text-with-end-marker)))  ; construct the compact suffix tree for the text
    (st-has-pattern? suffix-tree pattern)  ; use st-has-pattern? to check if the pattern exists in the suffix tree
    )
  )


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
  (let* ((st1 (text->cst (string->list (string-append (list->string text1) "$"))))  ; create a CST for text1
         (suffixes (sort (get-suffixes (string->list (list->string text2)))  ; generate and sort the suffixes of text2
                         (lambda (a b) (> (length a) (length b))))))  ; sort descending by length
    
    (let iter ((suffixes suffixes) (longest '()))  ; named let for iteration
      (if (null? suffixes)
          longest  ; return the longest common substring
          (let* ((suffix (car suffixes))
                 (match (find-match st1 suffix '())))  ; search the longest match in st1 for the current suffix
            (if (> (length match) (length longest))
                (iter (cdr suffixes) match)  ; update the longest substring if necesary
                (iter (cdr suffixes) longest)  ; continue searching for a longer substring
                )
            )
          )
      )
    )
  ) 


(define (find-match st suffix accumulated)
  (if (null? suffix)  ; if the current suffix is empty
      accumulated
      (let* ((branch (get-ch-branch st (car suffix))))  ; search a branch that starts with the 1st char of the suffix
        (if branch  ; if a branch exists
            (let* ((label (get-branch-label branch))  ; get the label of the branch
                   (subtree (get-branch-subtree branch))  ; get the subtree from that branch
                   (match-length (common-prefix-length suffix label)))  ; calculate the length of the common prefix
              (if (= match-length 0)  ; if no common prefix exists
                  (reverse accumulated)  ; return what accumulated so far
                  (find-match subtree  ; continue searching in the tree
                              (drop suffix match-length)  ; remove the common prefix from the suffix
                              (append accumulated (take label match-length))) ; add the prefix to the accum
                  )
              )  
            accumulated ; if no branch left, return the accumulated result
            )
        )
      )
  )  



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
  (define cst (text->cst (append text '(#\$)))) ; Convert text to a compact suffix tree
  
  ; helper function to search for the repeated substring
  (define (search-for-substring node accumulated)
    ; base case: if the node is empty, return false indicating no substring found
    (if (null? node)
        #f
        (let* ((branch (first-branch node))  ; extract the first branch from the node
               (label (car branch))  ; label of the branch
               (subtree (cdr branch))  ; subtree under the branch
               (new-accumulated (append accumulated label)) ; append label to the accumulated path
               )
          ; if the accumulated path has the requested length and there's a subtree to search into
          (if (and (not (null? subtree)) (>= (length new-accumulated) len))
              (take new-accumulated len)  ; return the substring if it meets the length requirement
              (let ((next-result (search-for-substring subtree new-accumulated)))  ; search in the subtree
                (if next-result
                    next-result  ; if found in the subtree, return it
                    (search-for-substring (other-branches node) accumulated)  ; else try the next branch
                    )
                )
              )
          )           
        )
                    
    )
  
  ; start the recursive search with the CST and an empty path
  (search-for-substring cst '())
  )