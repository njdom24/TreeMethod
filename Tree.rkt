#lang racket

;TODO: Combine terms and negated concl into one list, and use parser functions to split them
;Assumes terms is all ors, and concl is all ands (when negated)
(define (Tree terms concl)
  ;(traverse (cadr terms) (cadr (_not concl)));;assumes input rn
  ;(get-ands (append terms (_not concl)) '())
  (traverse (append (get-branches terms '()) (get-branches (_not concl) '()))
            (append (get-ands terms '()) (get-ands (_not concl) '()))
            )
)



(define (get-branches expr total)
  (if (null? expr)
      total
      (if (equal? (car expr) "or")
          (get-branches (cddr expr) (append total (cadr expr)))
          (get-branches (cddr expr) total)
      )
  )
)

(define (get-ands expr total)
  (if (null? expr)
      total
      (if (equal? (car expr) "and")
          (get-branches (cddr expr) (append total (cadr expr)))
          (get-branches (cddr expr) total)
      )
  )
)

(define (traverse branches ands)
  (if (null? branches)
      #t
      (and (traverse (cdr branches) ands) (exec-branch (append branches ands)))
      )
)

;(Tree (_implies (_or '("H" "W")) "M") (_or '("M" "H")))
;(Tree (_implies (_or "H" "W") "M") (_or "M" "H"))
(define (traverses branches ands)
  (if (null? branches)
      #t
      ;(and (exec-branch (append (car branches) ands)) (traverse (cdr branches) ands));;NEED TO CHECK FOR THE "OR" WHEN PASSING IN BRANCHES TO THE INITIAL CALL
      (exec-branch (append (cadar branches) ands));testing the first branch, assuming no further branching
      )
)

(define (exec-branch expr)
  (if (null? expr)
      #t
      (if (null? (extract-branches(clean-ands expr '()) '()));if no branches
          (check-conflicts (clean-ands expr '()))
          (check-conflicts (remove-branches(clean-ands expr '())))
          )
     
      )
  ;expr
  ;(check-conflicts (clean-ands expr '()));;;PUT A CHECK FOR ORS IN CHECK-CONFLICTS
);something about check-conflicts

(define (clean-ands lst return)
  (if (null? lst)
      return
      (if (list? (car lst))
          (if (equal? (caar lst) "and")
              (clean-ands (cdr lst) (append (cadar lst) return));add the nested and elements
              (clean-ands (cdr lst) (cons (car lst) return));add the element
              )
          ;skip over ors
          (clean-ands (cdr lst) (cons (car lst) return));add the or back in, evaluate later
          )
      )
)
;USED AFTER clean-ands
(define (remove-branches lst return)
  (if (null? lst)
      return
      (if (list? (car lst))
          (remove-branches (cdr lst) return);remove the or
          (remove-branches (cdr lst) (cons (car lst) return));add regular element back into list
          )
      )
)

(define (extract-branches lst return)
  (if (null? lst)
      return
      (if (list? (car lst))
          (extract-branches (cdr lst) (append (car lst) return));add or back into list
          (extract-branches (cdr lst) return);remove the regular element
          )
      )
)

;(clean-ands '(("and" ("!H" "!W")) "M" "!M" "!H") '())

;if the string has a ! in front, check if its next character exists in the list
;if no !, then check for a !, then check for a matching char
;returns true if no conflicts
(define (check-conflicts lst)
  (if (null? lst)
      #t
      (and (if (equal? #\! (car(string->list (car lst))));if string starts with negation
               (check-helper (list->string(list(cadr(string->list (car lst))))) (cdr lst));check if the letter is in the rest of the list
               (check-helper (string-append "!" (car lst)) (cdr lst));check if negated version is in the list
            )    
           (check-conflicts (cdr lst)))
      )
)

(define (clean-ors lst return)
  (if (null? lst)
      return
      (if (list? (car lst))
          (clean-ands (cdr lst) (append (cadar lst) return))
          (clean-ands (cdr lst) (cons (car lst) return))
          )
      )
)

(define (check-helper find total)
  (if (null? total)
      #t
      (if (equal? find (car total))
          #f
          (check-helper find (cdr total))
          )
      )
)

(define (_or . lst)
  (list "or" lst)
)

(define (_and . lst)
  (list "and" lst)
)

(define (_implies a b)
  (_or (_not a) b)
)

;(_and "A" "B" (_or "C" "D"))
;(_not (_and "A" "B" (_or "C" "D")))

(define (_not lst);flips the value of an expression
  (cond
    [(not (list? lst)) (negate lst)]
    [(equal? (car lst) "and") (list "or" (_not (cadr lst)))]
    [(equal? (car lst) "or") (list "and" (_not (cadr lst)))]
    [else (map negate lst)]
  )
)

(define (negate element);flips the value of a single element
  (if (list? element)
      (_not element)
      (if (equal? #\! (car(string->list element)))
          (substring element 1);remove negation
          (string-append "!" element);add negation
      )
   )
  
)