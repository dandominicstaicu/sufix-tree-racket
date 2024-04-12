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
  ; check if any word is empty or characters don't match
    [(or (null? w1) (null? w2) (not (char=? (car w1) (car w2))))
    ; if true, return list with reversed common pref and rest of words
     (list (reverse common) w1 w2)
     ]
    [else
    ; continue with the tails of the words and add the current character to the common prefix
     (find-longest-common-prefix (cdr w1) (cdr w2) (cons (car w1) common))
     ]
    )
  )

; The function `longest-common-prefix` takes two lists of characters (words) as input.
; It computes the longest common prefix between the two words.
; It returns a list containing three elements:
; 1. The longest common prefix as a list of characters.
; 2. The remainder of the first word after removing the common prefix.
; 3. The remainder of the second word after removing the common prefix.
; This implementation uses tail recursion for efficiency.
(define (longest-common-prefix w1 w2)
  (find-longest-common-prefix w1 w2 '()) ; call helper func
  )



; am schimbat, în numele funcției, cuvântul list în
; cuvântul collection

; The function `longest-common-prefix-of-collection` is designed to process a non-empty list of words
; that start with the same character. It calculates the longest common prefix among these words by
; recursively comparing and reducing the list until it converges on the longest common prefix.
; This approach stops the search as soon as it's guaranteed that the current common prefix is the final common prefix,
; optimizing the process by avoiding unnecessary comparisons once the maximum common prefix is identified.
(define (longest-common-prefix-of-collection words)
  (define (reduce-prefix words)
    ; check if the collection is empty or contains only one element
    (if (or (collection-empty? words) (collection-empty? (collection-rest words)))
        ; true -> the longest common prefix is the first word in the collection
        (collection-first words)
        ; else -> proceed to find the common prefix between the first two words
        (let* ((prefix-pair (longest-common-prefix (collection-first words) (collection-first (collection-rest words))))
               (common-prefix (car prefix-pair)))
          ; recursively call reduce-prefix with the new common prefix and the rest of the collection,
          ; reducing the collection size and building the common prefix
          (reduce-prefix (collection-cons common-prefix (collection-rest (collection-rest words))))
          )
        )
    )
  ; initial call
  (reduce-prefix words)
  )


; This function, `common-prefix-length`, is a utility function that computes the number of 
; characters at the beginning of two lists (representing strings or sequences of characters) 
; that match. It's intended to be used in the context of working with a suffix tree for pattern 
; matching in text. It facilitates determining how well a pattern matches a given label in the 
; tree by providing the length of their common prefix. This is crucial for navigating the suffix 
; tree to find if and where a pattern exists within the text the tree represents.
(define (common-prefix-length lst1 lst2)
  (define (helper l1 l2 count)
    ; if list is empty or the current characters do not match, return the current count
    (if (or (null? l1) ; check if first list is empty
            (null? l2) ; check if second list is empty
            (not (char=? (car l1) (car l2))))  ; check if the current characters do not match
        count  ; return the count of matching characters as the length of the common prefix
        ; else -> recursively call helper with the tails of the lists and increment count
        (helper (cdr l1) (cdr l2) (+ 1 count))
        )
    )
  ; call helper function with the initial values
  (helper lst1 lst2 0)
  )


; The function `match-pattern-with-label` serves as a crucial step in navigating a suffix tree to find whether
; and where a given pattern exists in the text represented by the tree. It evaluates the relationship between
; a pattern and a branch's label, determining whether the pattern is entirely contained within the label,
; partially matches indicating a need to search deeper, or doesn't match at all, providing a foundation for
; either concluding the search or proceeding to the next step based on the outcome.
(define (match-pattern-with-label st pattern)
  ; find a branch in the suffix tree that starts with the pattern's first character
  (let ((branch (get-ch-branch st (car pattern)))) ; find the branch based on the first character of the pattern
    (if (not branch) ; check if no branch starts with the pattern's first character
        (list #f '()) ; return false with an empty list if pattern not found
        ; if a branch is found, proceed to further analyze the match
        (let* ((label (get-branch-label branch)) ; retrieves the label of the found branch
               (subtree (get-branch-subtree branch)) ; retrieves the subtree under the branch
               (common-prefix-length (common-prefix-length pattern label))) ; calculate the common pref len between the pattern and the branch label
          ; decide on the match quality based on the common prefix length
          (cond
            ((equal? common-prefix-length (length pattern)) 
             #t) ; the pattern fully matches the label -> complete match
            ((equal? common-prefix-length (length label))
             ; check if the label is fully matched by the pattern, but the pattern may extend more than the label
             (if (= common-prefix-length 0)
                 (list #f '()) ; if there is no common prefix -> return false with an empty list
                 ; else -> return the label, the remaining unmatched part of the pattern, and the subtree for further searching
                 (list label (drop pattern common-prefix-length) subtree)))
            (else
             ; if there is a partial or no match, return false and the common prefix as far as it goes
             (list #f (take pattern common-prefix-length))
             )
            )
          )
        )
    )
  )

; This function, `st-has-pattern?`, navigates the suffix tree, starting from its root,
; trying to match the pattern against the labels of the branches. It leverages the tree's structure
; to efficiently determine whether the pattern is a substring of the text represented by the tree.
; Through recursive descent, it assesses matches at each node, either concluding the search when a
; match is confirmed or the possibility of a match is ruled out, or delving deeper into the tree where
; a partial match suggests the pattern may still be found.
(define (st-has-pattern? st pattern)
  (define (search-in-subtree subtree pattern)
    (cond
      ; if the pattern is empty == we've successfully matched all of it -> return true
      [(null? pattern) #t]
      ; if the subtree is empty and we still have pattern left == the pattern isn't found -> return false
      [(collection-empty? subtree) #f]
      ; else -> proceed with the search
      [else
       ; use match-pattern-with-label to check if the current subtree's label matches the pattern
       (let ((match-result (match-pattern-with-label subtree pattern)))
         (cond
           ; if the result is true == the pattern fully matches a label in the subtree -> the pattern exists
           [(eq? match-result #t) #t]
           ; if the match result is a list starting with false == the pattern doesn't match the label
           [(and (list? match-result) (eq? (first match-result) #f)) #f]
           ; if there's a partial match (list? match-result) -> continue searching in the subtree
           [(list? match-result)
            (search-in-subtree (caddr match-result) (cadr match-result))]
           ; else == any other unspecified cases (but all should be covered above)
           [else #f]
           )
         )
       ]
      )
    )

  ; start the search with the entire tree and the full pattern
  (search-in-subtree st pattern)
  )


; The `get-suffixes` function operates by taking a list of characters as input and producing a collection
; of all possible suffixes of that list. A suffix of a list is defined as any contiguous subsequence of 
; elements from the list that includes its end. The function achieves this by iteratively removing the first 
; element of the list to create each possible suffix, then accumulating these suffixes into a collection. 
; This can be particularly useful in contexts such as building a suffix tree, where access to all suffixes 
; of a text is necessary for the tree's construction.
(define (get-suffixes text)
  (define (suffixes-helper remaining-text)
    ; check if the current text segment is empty
    (if (null? remaining-text)
        (empty-collection) ; return an empty collection if there are no more characters
        ; else -> construct the collection of suffixes
        (collection-cons remaining-text
                         ; include the current segment as a suffix and recursively processes the rest
                         (suffixes-helper (cdr remaining-text))
                         )
        )
    )

  ; call the helper function
  (suffixes-helper text)
  )


; This function is useful for situations where you need to process or analyze only a subset of words from
; a larger collection based on their initial character. For instance, in the context of building a suffix tree
; or for any kind of lexical analysis, where grouping or categorizing words by their starting character can 
; significantly streamline further processing or analysis tasks.
(define (get-ch-words words ch)
  ;use the collection-filter function to apply a filter to the collection of words
  (collection-filter 
    (lambda (word) ; define an anonymous function to check each word
      (and 
        (not (collection-empty? word)) ; check if the word is not empty
        (char=? ch (collection-first word)) ; check if the first character of the word matches 'ch'
        )
      ) 
    words ; the collection of words to be filtered
    )
  )


; The `ast-func` function starts by extracting the first character from the first suffix in the collection
; to use as a label. It then processes the entire collection of suffixes, removing the first character from 
; each. The resulting collection (with shortened suffixes) and the label are then paired and returned. 
; This kind of operation is crucial in steps that involve building or updating the structure of a suffix tree, 
; where each node's label might represent a common prefix in a set of suffixes, and the children nodes 
; represent continuations of those suffixes minus the common prefix.
(define (ast-func suffixes)
  (let* ((first-suffix (collection-first suffixes)) ; get the first suffix from the stream
         (label (list (collection-first first-suffix))) ; the label is a list containing the first character of the first suffix
         (new-suffixes (collection-map collection-rest suffixes))) ; remove the first char from each suffix
    (cons label new-suffixes))) ; no need to convert, new-suffixes is already a stream


; The `cst-func` function is used for constructing compressed suffix trees, which are more space-efficient than standard suffix trees.
; By identifying the longest common prefix among all suffixes, it effectively reduces the redundancy in the representation of these suffixes,
; paving the way for a compressed structure. The resulting pair of the LCP and the modified suffixes serve as a basis for the next steps in
; building or updating the tree, contributing to its overall compression and efficiency.
(define (cst-func suffixes)

  ; find the longest common prefix among all suffixes in the collection
  (define (stream-longest-common-prefix suffix-stream)
    (if (collection-empty? suffix-stream)
        '() ; if the suffix stream is empty, return an empty list.
        ; Recursive case -> start with the first suffix and iterate through the rest
        (let loop ((pref (collection-first suffix-stream)) ; initialize with the first suffix
                   (rest-suffixes (collection-rest suffix-stream)))
          ; if there are no more suffixes -> return the accumulated prefix
          (if (collection-empty? rest-suffixes)
              pref
              ; else -> calculate the LCP with the next suffix
              (let* ((next-suffix (collection-first rest-suffixes))
                     (new-prefix (car (longest-common-prefix pref next-suffix)))) ; compute LCP with the next suffix
                ; recurse with the new prefix and the rest of the suffixes
                (loop new-prefix (collection-rest rest-suffixes))
                )
              )
          )
      )
  )

  ; use the longest common prefix among all suffixes as the label for this part of the tree
  (let* ((lcp (stream-longest-common-prefix suffixes))
         (label lcp)  ; the label is the LCP itself
         ; update each suffix by removing the LCP, preparing them for further processing
         (new-suffixes (collection-map (lambda (suffix)
                                         (collection-drop suffix (length lcp))) ; remove the LCP from each suffix
                                       suffixes)))
    ; return a pair of the label (LCP) and the updated collection of suffixes
    (cons label new-suffixes)
    )
  )


(define (not-false? val)
  (not (eq? val #f)))

; ; considerați că și parametrul alphabet este un flux
; ; (desigur, și suffixes este un flux, fiind o colecție
; ; de sufixe)

; The `suffixes->st` function builds a suffix tree from a list of text endings. It works by taking these endings and, one by one, 
; organizes them into a tree structure. This makes it easier to see how the text pieces connect. The function uses a special rule 
; (provided by the labeling function) to name each part of the tree, ensuring the tree accurately represents the text from start to finish. 
; It’s a clever way to map out the text, making it quick to navigate through different parts.
(define (suffixes->st labeling-func suffixes alphabet)
  ; if the alphabet is empty -> return an empty collection, indicating no further branches can be formed
  (if (collection-empty? alphabet)
      empty-collection 
      ; Recursive case -> process each character in the alphabet to form the tree's branches
      (let* ((branches (collection-map (lambda (ch)
                                         ; for each character, find all suffixes starting with that character
                                         (let* ((ch-suffixes (get-ch-words suffixes ch))  ; get all suffixes starting with ch
                                                ; apply the labeling function to these suffixes, if they exist
                                                (label-and-suffixes (if (not (collection-empty? ch-suffixes))
                                                                        (labeling-func ch-suffixes)
                                                                        #f)))
                                           ; if a label and suffixes were generated, proceed to form a branch
                                           (if label-and-suffixes
                                               (let* ((label (car label-and-suffixes))  ; extract the label from the result
                                                      (new-suffixes (cdr label-and-suffixes))  ; extract the new suffixes for the next level
                                                      ; recursively build the subtree for these new suffixes if they exist
                                                      (subtree (if (collection-empty? new-suffixes)
                                                                   empty-collection
                                                                   (suffixes->st labeling-func new-suffixes alphabet))))
                                                 ; form a branch as a pair of the label and the subtree
                                                 (cons label subtree))
                                               ; if no valid label or suffixes, indicate absence of a branch with #f
                                               #f)) 
                                         )
                                       alphabet)))
        ; filter out any false values from the branches, keeping only those that are valid.
        (collection-filter not-false? branches)
        )
      )
  )


; nu uitați să convertiți alfabetul într-un flux

; defines a higher-order function for constructing a suffix tree from text
(define (text->st labeling-func)
  ; returns a lambda function that takes the text and constructs a suffix tree
  (lambda (text)
    (let* [(text-with-end-marker (append text '(#\$)))  ; append an end marker to the text to signify its end
           (suffixes (get-suffixes text-with-end-marker))  ; generate all suffixes of the text, including the end marker
           ; create an alphabet list from the text, removing duplicates and sorting it
           (alphabet-list (sort (remove-duplicates text-with-end-marker) char<?))
           ; convert the sorted alphabet list into a collection (e.g., list, stream) for processing
           (alphabet-stream (list->collection alphabet-list))] 
      ; construct the suffix tree using the provided labeling function, the generated suffixes, and the alphabet stream
      (suffixes->st labeling-func suffixes alphabet-stream)
      )
    )
  )

; creates a specific suffix tree construction function using the ast-func labeling function
; this version will create a tree where each node represents all possible suffixes starting with a specific character
(define text->ast (text->st ast-func))

; creates another specific suffix tree construction function, this time using the cst-func labeling function
; this variant will create a compressed suffix tree, focusing on minimizing redundancy by merging common prefixes
(define text->cst (text->st cst-func))


; ; dacă ați respectat bariera de abstractizare,
; ; această funcție va rămâne nemodificată.

; This function essentially transforms the text into a searchable tree structure, then quickly navigates this structure to find if the pattern exists within the text.
; It's an efficient way to check for substrings without scanning the entire text linearly.
(define (substring? text pattern)
  ; start by adding a unique end marker to the text. This ensures that we can distinguish between similar substrings and the end of the text
  (let* ((text-with-end-marker (append text '(#\$)))  ; append a unique marker to signify the end of the text
         ; build a atomic suffix tree for the text including the end marker
         (suffix-tree (text->ast text-with-end-marker)))  ; use the text->ast function to create the suffix tree
    ; check if the pattern exists in the suffix tree. The st-has-pattern? function looks through the tree to find if the pattern can be traced as a path from the root.
    (st-has-pattern? suffix-tree pattern)  ; determine if the pattern is a substring of the original text
    )
  )


; ; dacă ați respectat bariera de abstractizare,
; ; această funcție va rămâne nemodificată.

; The key idea of this function is to traverse the CST, collecting labels from the branches until a substring of the desired length is found.
; This approach leverages the efficient structure of the CST to find repeated substrings without examining every possible substring manually.
; This function searches for a repeated substring of a specific length within a given text.
(define (repeated-substring-of-given-length text len)
  ; convert the text into a compact suffix tree (CST) by appending a unique end marker to ensure all suffixes are represente
  (define cst
    (text->cst (append text '(#\$)))  ; append a unique marker to the text to create a suffix tree, ensuring every suffix is accounted for
    ) 

  ; define a helper function to recursively search for the repeated substring within the CST
  (define (search-for-substring node accumulated)
    ; Base case -> if the current node is empty, indicating no further branches to explore, return false
    (if (collection-empty? node)
        #f
        ; recursive case -> Process the first branch of the current node.
        (let* ((branch (collection-first node))  ; get the first branch of the node.
               (label (collection->list (get-branch-label branch)))  ; Convert the branch label from a collection or stream to a list for easy manipulation.
               (subtree (get-branch-subtree branch))  ; get the subtree rooted at the current branch.
               ; append the current label to the accumulated path so far.
               (new-accumulated (append accumulated label))) 
          (cond
            ; if the length of the accumulated labels meets or exceeds the desired length, and there is more tree to explore,
            ((and (>= (length new-accumulated) len)
                  (not (collection-empty? subtree)))
             (take new-accumulated len))  ; return the substring of the accumulated labels up to the specified length.
            ; else -> continue the search recursively.
            (else
             (let ((next-result (search-for-substring subtree new-accumulated)))
               (or next-result  ; if a match is found in the subtree, use it.
                   (search-for-substring (collection-rest node) accumulated)  ; else -> move to the next sibling branch in the tree
                   )
               )
             )
            )
          )
        )
    ) 

  ; start the search by calling the helper function with the CST and an empty list to accumulate the labels.
  (search-for-substring cst '())
  )


