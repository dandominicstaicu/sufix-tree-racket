#lang racket

; TODO watch this! https://youtu.be/quxwaX8Z3O8?si=-jJDBMusWNxp_a9q

(require "suffix-tree-stream.rkt")
(require "collection.rkt")

(provide (all-defined-out))

;; Vom prelua toate funcțiile din etapele 1-3 (exceptând
;; longest-common-substring, care nu beneficiază de 
;; reprezentarea ca flux întrucât parcurge tot arborele)
;; și le vom adapta la noua reprezentare a unui ST.
;;
;; Pentru că un ST este construit pornind de la o colecție
;; de sufixe și pentru că ne dorim să nu calculăm toate
;; sufixele decât dacă este nevoie, vom modifica toate
;; funcțiile care prelucrau liste de sufixe pentru a
;; prelucra fluxuri de sufixe.
;;
;; Obs: fără această modificare a listelor de sufixe în
;; fluxuri de sufixe, și presupunând că am manipulat
;; arborii de sufixe doar prin interfața definită în
;; fișierul suffix-tree (respectând astfel bariera de 
;; abstractizare), ar trebui să alterăm doar funcția 
;; suffixes->st care este practic un constructor pentru
;; tipul ST.
;; Din cauza transformării listelor de sufixe în fluxuri,
;; avem mult mai multe implementări de modificat.
;; Puteam evita acest lucru? Da, utilizând conceptul de
;; colecție de sufixe de la început (în loc să presupunem
;; că ele vor fi prelucrate ca liste). În loc de cons,
;; car, cdr, map, filter, etc. am fi folosit de fiecare
;; dată collection-cons, collection-first, ... etc. -
;; aceste funcții fiind definite într-o bibliotecă
;; inițială ca fiind echivalentele lor pe liste, și
;; redefinite ulterior în stream-cons, stream-first,
;; ... etc. Operatorii pe colecții de sufixe ar fi 
;; folosit, desigur, doar funcții de tip collection-.
;;
;; Am ales să nu procedăm astfel pentru că ar fi provocat
;; confuzie la momentul respectiv (când chiar operatorii
;; pe liste erau o noutate) și pentru a vă da ocazia să
;; faceți singuri acest "re-design".


; TODO
; Copiați din etapele anterioare implementările funcțiilor
; de mai jos și modificați-le astfel:
; - Toate funcțiile care lucrează cu liste de sufixe vor
;   lucra cu un nou tip de date Collection, ai cărui
;   constructori și operatori vor fi definiți de voi
;   în fișierul collection.rkt.
; - Pentru toate funcțiile, trebuie să vă asigurați că
;   este respectată bariera de abstractizare (atât în 
;   cazul tipului ST cât și în cazul tipului Collection).
; Obs: cu cât mai multe funcții rămân nemodificate, cu atât
; este mai bine (înseamnă că design-ul inițial a fost bun).

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



; am schimbat, în numele funcției, cuvântul list în
; cuvântul collection

(define (longest-common-prefix-of-collection words)
  (define (reduce-prefix words)
    (if (or (collection-empty? words) (collection-empty? (collection-rest words)))
        (collection-first words)
        (let* ((prefix-pair (longest-common-prefix (collection-first words) (collection-first (collection-rest words))))
               (common-prefix (car prefix-pair)))
          (reduce-prefix (collection-cons common-prefix (collection-rest (collection-rest words))))
          )
        )
    )
  (reduce-prefix words)
  )


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
  (let ((branch (get-ch-branch st (car pattern)))) ; Assumes get-ch-branch can handle the stream/collection
    (if (not branch) ; If no branch starts with the pattern's first character
        (list #f '()) ; Pattern not found in the text
        (let* ((label (get-branch-label branch)) ; Assumes get-branch-label works with the stream/collection
               (subtree (get-branch-subtree branch)) ; Assumes get-branch-subtree works with stream/collection
               (common-prefix-length (common-prefix-length pattern label)))
          (cond
            ((equal? common-prefix-length (length pattern))
             #t) ; Fully matches the start of the label
            ((equal? common-prefix-length (length label))
             (if (= common-prefix-length 0)
                 (list #f '()) ; No common prefix
                 (list label (drop pattern common-prefix-length) subtree))) ; Partial match, need to search deeper
            (else
             (list #f (take pattern common-prefix-length)) ; Partial or no match, return common prefix
             )
            )
          )
        )
    )
  )


(define (st-has-pattern? st pattern)
  (define (search-in-subtree subtree pattern)
    (cond
      [(null? pattern) #t]  ; If the pattern is empty, it's found.
      [(collection-empty? subtree) #f]  ; If the subtree is empty, the pattern is not found.
      [else
       (let ((match-result (match-pattern-with-label subtree pattern)))
         (cond
           [(eq? match-result #t) #t]  ; Fully matched within a label.
           [(and (list? match-result) (eq? (first match-result) #f)) #f]  ; Does not match label.
           [(list? match-result)
            (search-in-subtree (third match-result) (second match-result))]  ; Partial match, continue in the subtree.
           [else #f] ; Case for completeness, though covered by the conditions above.
           )
         )
       ]
      )
    )  
  (search-in-subtree st pattern)
  )



(define (get-suffixes text)
  (define (suffixes-helper remaining-text)
    (if (null? remaining-text)
        (empty-collection) ; Return an empty stream
        (collection-cons remaining-text
                         (suffixes-helper (cdr remaining-text))
                         )
        )
    )
  (suffixes-helper text)
  )



(define (get-ch-words words ch)
  (collection-filter (lambda (word)
                       (and (not (collection-empty? word)) ; check if a collection is empty
                            (char=? ch (collection-first word)) ; gets the first element of a collection
                            )
                       ) 
                     words
                     )
  )


(define (ast-func suffixes)
  (let* ((first-suffix (collection-first suffixes)) ; Get the first suffix from the stream
         (label (list (collection-first first-suffix))) ; The label is a list containing the first character of the first suffix
         (new-suffixes (collection-map collection-rest suffixes))) ; Remove the first char from each suffix
    (cons label new-suffixes))) ; No need to convert, new-suffixes is already a stream


(define (stream-longest-common-prefix suffix-stream)
  (if (collection-empty? suffix-stream)
      '() ; Return an empty list if the stream is empty
      (let loop ((pref (collection-first suffix-stream)) ; Initialize with the first suffix
                 (rest-suffixes (collection-rest suffix-stream)))
        (if (collection-empty? rest-suffixes)
            pref ; Return the accumulated prefix if there are no more suffixes
            (let* ((next-suffix (collection-first rest-suffixes))
                   (new-prefix (car (longest-common-prefix pref next-suffix)))) ; Compute LCP with the next suffix 
              (loop new-prefix (collection-rest rest-suffixes)) ; Recurse with the new prefix and the rest of the suffixes
              )
            )
        )
      )
  )


(define (cst-func suffixes)
  (let* ((lcp (stream-longest-common-prefix suffixes))  ; Assume this function is implemented for streams
         (label lcp)  ; The label is the LCP itself
         (new-suffixes (collection-map (lambda (suffix)
                                         (collection-drop suffix (length lcp))) ; Remove the LCP from each suffix
                                       suffixes)))
    (cons label new-suffixes) ; Return the pair of the label and new suffixes as streams
    )
  )


(define (not-false? val)
  (not (eq? val #f)))

; ; considerați că și parametrul alphabet este un flux
; ; (desigur, și suffixes este un flux, fiind o colecție
; ; de sufixe)
(define (suffixes->st labeling-func suffixes alphabet)
  (if (collection-empty? alphabet)
      empty-collection  ; If the alphabet is empty, return an empty stream
      (let* ((branches (collection-map (lambda (ch)
                                     (let* ((ch-suffixes (get-ch-words suffixes ch))  ; Lazily get all suffixes starting with ch
                                            (label-and-suffixes (if (not (collection-empty? ch-suffixes))
                                                                    (labeling-func ch-suffixes)
                                                                    #f)))  ; Apply labeling function only if ch-suffixes is not empty
                                       (if label-and-suffixes
                                           (let* ((label (car label-and-suffixes))  ; Extract label
                                                  (new-suffixes (cdr label-and-suffixes))  ; Extract new suffixes for subtree
                                                  (subtree (if (collection-empty? new-suffixes)
                                                               empty-collection
                                                               (suffixes->st labeling-func new-suffixes alphabet)))  ; Recursive call for subtree
                                                  )
                                             (cons label subtree)  ; form a branch as a pair
                                             )
                                           #f ; Use #f to indicate failure or absence of a branch
                                           ) 
                                       )
                                     )
                                   alphabet)))
        (collection-filter not-false? branches)  ; Filter out false values, keeping only valid branches
        )
      )
  ) 


; nu uitați să convertiți alfabetul într-un flux
(define (text->st labeling-func)
  (lambda (text)
    (let* [(text-with-end-marker (append text '(#\$)))  ; Add end marker as a character
           (suffixes (get-suffixes text-with-end-marker))  ; get-suffixes now returns a stream
           (alphabet-list (sort (remove-duplicates text-with-end-marker) char<?))  ; Sort and remove duplicates
           (alphabet-stream (list->collection alphabet-list)) ; Convert the sorted alphabet list into a stream
           ] 
      (suffixes->st labeling-func suffixes alphabet-stream)
      )
    )
  )


(define text->ast (text->st ast-func))

(define text->cst (text->st cst-func))

; ; dacă ați respectat bariera de abstractizare,
; ; această funcție va rămâne nemodificată.
(define (substring? text pattern)
  (let* ((text-with-end-marker (append text '(#\$)))  ; text ends with a unique marker
         (suffix-tree (text->ast text-with-end-marker)))  ; construct the compact suffix tree for the text
    (st-has-pattern? suffix-tree pattern)  ; use st-has-pattern? to check if the pattern exists in the suffix tree
    )
  )



; ; dacă ați respectat bariera de abstractizare,
; ; această funcție va rămâne nemodificată.
; (define (repeated-substring-of-given-length text len)
;   'code)
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
