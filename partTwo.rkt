; Jesse Clegg

; Takes in a list and an atom
; then removes all occurances of that atom, no matter how deeply nested
(define (deleteAtom lst item)
  (cond ((null? lst);base case = empty list
         '()
        )
        ; If first element in list = item to delete
        ; then dont reconstruct with that first element, as we want to delete the car in this case, pass rest of list(cdr) into deleteAtom function
        ((equal? item (car lst))
         (deleteAtom (cdr lst) item)
        )
        ; If the car is a list
        ; then check all sublists for occurances of the atom and delete any found
        ((list? (car lst))
         (cons (deleteAtom (car lst) item) (deleteAtom (cdr lst) item));rebuilds sublists without the element to delete
        )
        ; If car of the list is not = the element to delete,
        ; then continue walking the cdr of the list, passing cdr of each successive list until the whole original list has been walked
        ; then rebuild the returns from each recursive call while preserving order
        (else
         (cons (car lst) (deleteAtom (cdr lst) item));because of the cons, car will be kept since it doesnt match the atom to delete
        )
  )
)

; Takes two atoms and a list as paramters
; replaces all instances of the current atom in this list with the new atom, no matter how deeply nested
(define (swapTwo current new lst)
  (cond
        ; Base case = empty list
        ((null? lst)
         '()
        )
        ; If the car of the list is another sublist, pass that sublist in to a seperate recursive call
        ; cdr of this list gets its own function call, then rebuild the results of both recursive calls while preserving list order
        ((list? (car lst))
         (cons (swapTwo current new (car lst)) (swapTwo current new (cdr lst)))
        )
        ; If irst element = atom to replace 
        ; then rebuild with the new atom after recursive calls on the cdr of this list return
        ((equal? current (car lst))
        (cons new (swapTwo current new (cdr lst)))
        )
        ; Default is to continue walking list by passing cdr of current list into same function until list is empty,
        ; then rebuild the returns of each function call while preserving order of original list
        (else
        (cons (car lst) (swapTwo current new (cdr lst)))
        )

  )

)


(display (deleteAtom '(4 5 6 7 69) 69));any use of display proves a list is returned
(newline)
(deleteAtom '(4 (5 6) 7 () 69) 69)
(deleteAtom '(4 (5 6) 7 () 69) 6)
(deleteAtom '(bill mary sue william) 'sue)
(deleteAtom '(bill (mary sue) william) 'sue)
(deleteAtom '(5 () 5000 -3 bob) -3)
(deleteAtom '(5 () 5000 -3 bob) 5000)
(deleteAtom '() 7)
(deleteAtom '(4 6 23 900 72) 1)
(deleteAtom '(4 (7 (6 (9 4))) 79 0 4) 4)
(deleteAtom '(a b c d a c a a) 'a)
(deleteAtom '((a b) c () d a c a a) 'a)
(deleteAtom '(((a (a)) b) c () d a c a a) 'a)
(deleteAtom '(a b c d a c (a ((a)))a a) 'a)
(newline)
(newline)

(display (swapTwo 7 400 '(4 7 9 400 7 14)))
(newline)
(swapTwo 1 5 '(1))
(swapTwo 1 5 '(5))
(swapTwo 7  50 '(1000 67 800 (9) 10 700 8))
(swapTwo 7 400 '(4 7 9 (69 95 7) 400 7 14))
(swapTwo 'a 'b '(a b c d a c a a))
(swapTwo 'a 'b '((a b) c () d a c a a))
(swapTwo 'a 'b '(((a (a)) b) c () d a c a a))
(swapTwo 'a 'b '(a b c d a c (a ((a)))a a))
(swapTwo 'bill 'william '(bill bill (bill sue) william bill))
(swapTwo 'a 'b '())