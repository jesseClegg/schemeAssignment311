; Jesse Clegg

; Takes in a list
; returns that list sorted by implementing a recursive bubble sort algorithm
(define (bubbleSort lst)
  ; Pass this list into the outter loop function of a typical bubblesort algorithm
  ; passses the length of this list to calculate the number of iterations for the inner loop to guarantee a sorted list 
  (outterLoop lst (length lst))
)

; Function that acts like the outter loop of bubblesort
; performs the inner loop sort on an updated list each time
; length of list dictates inner loop be called n-1 times with n being list size
; each functional call decrements n by one, until n=1
(define (outterLoop lst n)
  (cond
    ;list has been passed into inner loop n-1 times, guaranteed sorted, return this sorted list
    ((= n 1)
     lst
     )
   ;decrement n with each function call until list has been walked n-1 times
   (else
    (outterLoop (innerLoop lst) (- n 1))
   )
  )
)  

; Function that performs the tasks of the inner loop of bubble sort
; starts with the first element of a list as the index, compares to each element at n+1 position i.e. cadr of each list
; if n+1 is greater, n+1 and index are swapped
; else continue walking list by passing cdr of the list into this recursive function 
(define (innerLoop lst)
  (cond
       ; Case of last item in list
       ; No need to sort a list of one element
       ((= 1 (length lst))
       lst
        )
       ; If first>second (yes swap)
       ((> (car lst) (cadr lst))
        (cons (cadr lst) (innerLoop (cons (car lst) (cddr lst))))
        )
       ; Default case, i.e. first and second are =, or first<second, thus no swap
       ; Continue walking the list by passing cdr into innerLoop
       ; then rebuild list in order after recursive calls return
       (else
        (cons (car lst) (innerLoop (cdr lst)))
       )
 )
)


(display (bubbleSort '(10 9 8 7 6 5 4 3 2 1)));proves a list has been returned
(newline)
(bubbleSort '(7 87.87 100 7 6 9))
(bubbleSort '(4 2 9 7))
(bubbleSort '(777777 5 8 1 12 500))
(bubbleSort '(7 6 100 7 6 9))
(bubbleSort '(100 -500 600 1 0))
(bubbleSort '(-3 -6 -2 1 9 0 -8 -1))
(bubbleSort '(3.14 0 -3.14))
(bubbleSort '(100 100 100 7 100 7))
(bubbleSort '(1 0.9 0.8 0.7 0.3 0.6 0.5 0.4 0.1 0 0.2))
(bubbleSort '(20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1 0))
