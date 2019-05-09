#lang racket
;negate the last one
(define (tree-helper lst return)
  (if (null? (cdr lst));one element left
      (if (list? (car lst))
          (append return (list (_not (car lst))))
          (append return (list(_not (car lst))))
          )
      (tree-helper (cdr lst) (append return (list(car lst))))
      )
)
;(tree-helper '("A" "B" "C" ("or" ("D" "B")) "D") '())
(define (tree . lst)
  (exec (tree-helper lst '()) '())
)

(define (tree1 . lst)
  (tree-helper lst '())
)

;(tree (_implies (_or "H" "W") "M") (_or "M" "H"))
;(tree (_implies (_or "H" "W") "M") "M")

;> (exec '(("or" ("!B" "!D")) "C" "B" "A") '())
;#f
;> (exec '("!B" "C" "B" "A") '())
;#f
;> (exec '("D" "C" "B" "A") '())
;#t

(define (exec lst totals)
  (if (null? lst)
      (check-conflicts totals);return true if no conflicts
      (if (list? (car lst))
          (if (equal? (caar lst) "or")
              (branch (cadar lst) (cdr lst) totals)
              (if (equal? (caar lst) "and")
                  (exec (append (cdr lst)(cadar lst)) totals)
                  (exec (append lst(cadar lst)) totals)
                  )
              
              )
              (exec (cdr lst) (append totals (list(car lst))))
          )
      )
)


;(list (_or "P" (_or "Q" "!R")) (_implies "P" "!R") (_implies "Q" "!R") "R")
;(tree '("or" ("P" ("or" ("Q" "!R")))) '("or" ("!P" "!R")) '("or" ("!Q" "!R")) "!R")
(define (branch2 sublst lst totals)
  (if (null? sublst)
      #t
      (and (branch (cdr sublst) lst totals)
           (if (list? (car sublst))
               (exec (append (car sublst) lst) totals)
               (exec lst (append totals (list(car sublst))))
               ))
      )
)

(define (branch sublst lst totals)
  (if (null? sublst)
      #t
      (if (list? (car sublst))
          (and (exec (append lst (list (car sublst))) totals)
               (branch (cdr sublst) lst totals))
          (and (exec lst (append totals (list(car sublst))))
               (branch (cdr sublst) lst totals))
          )
      )
)

;(tree (_implies (_or "H" "W") "M") (_or "M" "H"))
;'("or" (("and" ("!H" "!W")) "M"))
;'("and" ("!M" "!H"))

;if the string has a ! in front, check if its next character exists in the list
;if no !, then check for a !, then check for a matching char
;returns true if no conflicts
(define (check-conflicts2 lst)
  lst
)
(define (check-conflicts lst)
  (not (conflict-helper lst))
)

(define (conflict-helper lst)
  (if (null? lst)
      #t
      (and (if (equal? #\! (car(string->list (car lst))));if string starts with negation
               (check-helper (list->string(list(cadr(string->list (car lst))))) (cdr lst));check if the letter is in the rest of the list
               (check-helper (string-append "!" (car lst)) (cdr lst));check if negated version is in the list
            )    
           (conflict-helper (cdr lst)))
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