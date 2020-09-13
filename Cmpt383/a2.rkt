#lang racket
;;Assignment 2
;;Jia Ming Wu
;;301278354

;;1:
(define (is-digit? c)
  (cond [(not(char? c)) #f] ;;check literal
        [(and (> (char->integer c) 47) (< (char->integer c) 58)) #t ] ;;ensure char are 0 - 9
        [else #f]))
 
;;2

(define (is-letter? c)
  (cond [(not(char? c)) #f] ;;check literal
        [(and (> (char->integer c) 64) (< (char->integer c) 91)) #t ] ;;ensure char are a-z AND A-Z
        [(and (> (char->integer c) 96) (< (char->integer c) 123)) #t ]
        [else #f]))


;;3:


(define (is-letterordigit? c)
  (cond [(not(char? c)) #f] ;;check literal
        [(and (> (char->integer c) 47) (< (char->integer c) 58)) #t ] ;;ensure char are 0 - 9
        [(and (> (char->integer c) 64) (< (char->integer c) 91)) #t ] ;;ensure char are a-z AND A-Z
        [(and (> (char->integer c) 96) (< (char->integer c) 123)) #t ]
        [else #f]))


(define (is-vble? x)
  (cond [(not(symbol? x)) #f]
        [(eq? 't x) #f]
        [(eq? 'f x) #f]
        [(set! x(symbol->string x)) ;;change to string
         (set! x(string->list x)) ;;change to list
         (if (is-digit? (first x)) #f 
             (andmap is-letterordigit? (rest x)))]))


;;4
;;is it a true value
(define (is-truev? x)
  (cond [(eq? 't x) #t]
        [(eq? 'f x) #t]
        [else #f]
        ))

;;check if every element is a true value or variable
(define (is-bool-env? x)
  (cond [(not (list? x)) #f]
        [(not (pair? x)) #f]
        [(not (list? (car x))) #f]
        [(eq? #t (and (andmap is-truev? (flatten (map cdr x)))
                      (andmap is-vble? (map car x)))) #t]    
        [else #f]
        ))


;;5
(define (is-bool-expr? expr)
  ;; is it a variable then #t, else simplify and recursive
  (if(is-vble? expr) #t
     (match expr
       ['t #t]
       ['f #t]
    
       [`(not ,a) (is-bool-expr? a)]
       [`(,a or ,b) (and (is-bool-expr? a)
                      (is-bool-expr? b))]
       [`(,a and ,b) (and (is-bool-expr? a)
                       (is-bool-expr? b))]
       [`(,a --> ,b) (and (is-bool-expr? a)
                       (is-bool-expr? b))]
    
       [_ #f])))


;;6
(define (eval-bool-expr expr env)
  (cond

    ;; if the variable cant not define
    [(and (is-vble? expr)
          (not (eq? #t expr))
          (not (eq? #f expr))
          (eq? #f (assoc expr env)))
     (error "eval-bool: undefined variable")]

    ;;if the variable is in the env 
    [(is-vble? expr)
     (eval-bool-expr (last (assoc expr env)) env)]

    ;;solve
    [else(match expr
       ['t #t]
       ['f #f]
       [`(not ,a) (not (eval-bool-expr a env))]
       [`(,a or ,b) (or (eval-bool-expr a env)
                        (eval-bool-expr b env))]
       [`(,a and ,b) (and (eval-bool-expr a env)
                          (eval-bool-expr b env))]
       [`(,a --> ,b) (implies (eval-bool-expr a env)
                              (eval-bool-expr b env))]
       [_ (error "eval-bool syntax error")])]))

;;if the eval-bool-expr return #t or #f then output 't 'f
(define (eval-bool expr env)
  (if (eval-bool-expr expr env) 't 'f))


  


;;7

;;check if the word is and or and not
(define is-words? (lambda (e)
                    (and (is-vble? e)
                         (not(equal? e 'and))
                         (not(equal? e 'or))
                         (not(equal? e 'not)))))

(define (get-vbles expr)
  (remove-duplicates   
   (filter is-words?
           (flatten expr))))



;;8
(define (makelist n lst)
  (define num-sublst (expt 2 (- n 1)));;1->1 2->2 3->4 4->8
  (append (map append
               (make-list  num-sublst '(t))lst) ;;insert truth val
          (map append
               (make-list  num-sublst '(f)) lst))) ;;insert flase val

(define (all-truth-values n)
  (if (= n 1) ;;when n is only 1
      (makelist n '(())) ;;create empty list for 1
      (makelist n (all-truth-values (- n 1))))) ;; else recursive


;;9

;;checking a list having correct sublist (var true-val)
;;using filter later to filter
(define (checka lst)
  (cond
    ((and (is-vble? (first lst)) (is-truev? (second lst))) #t)
    (else #f)
    ))

;;checking a list having all the duplicates var
;;using filter later to filter
(define (checkb lst)
  (cond
    ((not (eq? (first (first lst)) (first (second lst)))) #t)
    (else #f)
    ))


;;(combinations giving me all the possible combinations
;;(permutations giving me all the possible permutations 

(define (sat expr)


  (define env '()) ;;init empty lst
  
  (ormap (lambda (arg) ;;ormap to check every arg within the possible list 
              (cond
                ((eval-bool-expr expr arg) (set! env  arg))) ;;if eval bool is true then set arg to env
              )
         (filter checkb  ;;using filter and check b to filter out all the duplicates sublists
                 (combinations
                  (filter checka   ;;use filter and check a to filter out all the correct sublists after combinations
                          (combinations   
                           (reverse (cons 't (cons 'f (get-vbles expr)))) 2))2)));;make a list with variable and truth val( var var ... t f)
  
  (if (empty? (append*( list env)))
          (error"'unsat")
          (append*( list env))));;if env is still empty which mean no env is found, output unsat

  
  
  

      
  
       
  
  




                   


  


         
        
        
        
 
        