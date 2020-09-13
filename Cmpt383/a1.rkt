#lang racket

;;Assignment 1
;;Jia Ming Wu
;;301278354


;;question 1
;;(my-length lst) returns the number of elements in a list
(define (my-length lst)
  ;;base case
  (if (empty? lst) 0
      ;;recursive
      (+ 1 (my-length (rest lst))))) 

;;question 2
;;(onion n) returns a list with n copies of the empty list embedded in it,
(define (onion n)
  (cond [(= 0 n) '()] 
        [(negative? n) (error ". . onion: n negative")] 
        [else (make-list 1 (onion (- n 1)))]))

;;question 3
;;(my-last lst) returns the last element of a list
(define (my-last lst)
  (cond
        [(empty? lst) (error ". . my-last: empty list")] 
        [(null? (rest lst)) (first lst)]
        [else (my-last (rest lst))]))
      
;;question 4
;;(double-up lst) returns a new list that contains all the elements on lst repeat
(define (double-up lst)
  (if (null? lst) '()
      (cons (first lst)
            (cons (first lst)
                  (double-up (rest lst))))))


;;question 5
;;call functions flatten
(define (my-flatten x)
  (cond [(not (list? x)) x]
        [(empty? x) '()]
        [(list? (first x))
         (append (my-flatten (first x))
                 (my-flatten (rest x)))]
        [else (cons (first x)
                      (my-flatten (rest x)))]))

;;swap the first and last element
(define (swap-end lst)
  ;;check list only have 0 or 1 element, if true return lst
  (cond ((or (null? lst)
             (null? (cdr lst))) lst)
        (else  
         (my-flatten
          ;;first element
          (cons (last lst)
                ;;middle elements
                (cons (reverse (rest (reverse (cdr lst))))
                      ;;last elements
                      (cons (first lst)              
                            '())))))))



;; question 6
;; (softmax lst) returns a new list that is the standard (unit) softmax of lst.
;;Assume lst is a non-empty list of numbers (which can be positive, negative, or 0).
;;The numbers in the list returned by (softmax lst) are all in the range [0,1], and sum to 1.

;; sum function to sum up total exp value of values in the list 
(define (sum lst)
  (if (empty? lst) 0
      (+ (exp (first lst)) (sum (rest lst))))
  )

;;divide every element in list by x
(define (dividelist lst x)
  (map (lambda (n) (/ n x)) lst))

;;exp every element in list
(define (explist lst)
  (map (lambda (n) (exp n)) lst))


(define (softmax lst)
  (cond [(empty? lst) (error ". . softmax: lst is empty")] ; error output
        [else (dividelist (explist lst) (sum lst))]))


;; question 7
;;(is-relation? lst) returns #t if, and only if, lst is a non-empty list of lists of the form (a b), where both a and b are integers. None of the pairs should be repeated.
(define (is-relation? lst)
  (cond
        [(not(list? lst)) #f]
        [(empty? lst)  print #f]
        [(not(andmap integer? (my-flatten lst))) #f]
        [(odd? (my-length(my-flatten lst))) #f]
        [(= 1 (my-length lst) ) #t]
        [(odd? (my-length lst)) print #f]
        [else #t]))

;;question 8

;;collect all odd index elements
(define (odds lst)
  (if (empty? lst) '()
      (cons (first (first lst)) (odds(rest lst)))))

;;collect all even index elements
(define (evens lst)
  (if (empty? lst) '()
      (cons (last (first lst)) (evens(rest lst)))))

;;compare all element within list
(define (compare lst)
    (if (pair? lst)
        (and (not(member (first lst) (rest lst)))
             (compare (rest lst)))#t))

;;(is-function? lst) returns #t if, and only if, lst is a relation, and all of its first elements are unique and all of its second elements are unique. 
(define (is-function? lst)
  (cond
        [(not(is-relation? lst)) #f ]
        [(empty? lst) '()]
        [(not(list? lst)) #f]
        [(not(compare (evens lst))) #f]
        [(not(compare (odds lst))) #f]
        [else #t]))

         
        
                  
;;Atom 1
(define (atom? x)
  (cond
    [(or (symbol? x)(number? x)) #t]
    [else #f]))
    
;;atom 2
(define (count-atoms1 lst)
  (cond [(empty? lst) 0]
        [(atom? (first lst)) (+ 1 (count-atoms1 (rest lst)))]
        [else (count-atoms1 (rest lst))]))
  
;;atom 3                           
(define (count-atoms2 lst)
   (my-length (filter atom? lst)))

;;atom 4

(define test-suite
  '((() 0)
    ((a) 1)
    ((2) 1)
    (("a") 0)
    ((a b) 2)
    ((3 1) 2)
    ((a 2) 2)
    (("a" b) 1)
    ((1 2 3 4) 4)
    ((a b c d) 4)
    ((1 a 2 3 b c) 6)
    ((1 a () 2 3 "m" b c (4 s 2)) 6)))


;;set a right count atoms solution for compare
(define (count-atoms lst)
  (my-length (filter atom? lst)))


(define (check f suite)
  (cond
         [(empty? suite) '()]
         ;;print result when they dont equal
         [(not(equal? (f (first suite))
                      (count-atoms (first suite))))
          (cons (format "fail: (f '~A) returned ~S; expected ~V "
                       ;;only wants to print the first (a) in ((a) 1) 
                       (first(first suite))
                       ;;f return
                       (f (first(first suite)))
                       ;;expected return 
                       (count-atoms (first (first suite))))
               (check f (rest suite)))]

         [else print "all test passed"]))
  
                           
                     
     
  
       
      
                  



        
       

  
      



                     
      

        

