; Jesse Clegg

; Take in a list and return the list with all elements formatted to absolute value 
(define (getAbs lst)
  ; Base case = has walked entire list
  ; meaning an empty list has been passed in, so return an empty list "'()"
  (if (null? lst)
         '()
  ;rebuild list with the first element in abs form added to the cdr of this list, after recursive calls return formatted cdr of each respective list
  ;preserves list order 
  (cons (abs(car lst)) (getAbs (cdr lst)))
  )
)

(display (getAbs '(0 5.56 4 -3 -700 4000 69)));use of display proves a list is returned
(newline);courtesy to grader for readability purposes
(getAbs '(3 4 69 70000.4 -7))
(getAbs '());empty list case
(getAbs '(0 -0 500000 65 -12.3));decimal case
(getAbs '(70 -700 70000 -700000 7 -7))
(getAbs '(-1 -2 -3 -4 -5 -6 -7 -8 -9 -10));demonstrate preservation of list order
(getAbs '(10 9 8 7 6 5 4 3 2 1 0));demonstrate preservation of list order
(getAbs '(-5 7 6000 -12.6 12))
(getAbs '(-0));zero case
(getAbs '(-1 -2 -3 4 -5 -6 -7 -8 9 10))
(getAbs '(10 -9 8 -7 6 -5 4 -3 2 -1 0))