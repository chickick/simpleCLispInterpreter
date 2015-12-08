#|
NAME:       JINXIN HE 
|#

#|
test cases:   
(fl-interp '(+ (a 3) (b 2 3)) '( (b X Y = (+ X Y))  (A W = (+ 1 W))) )   --->  9 
(fl-interp '(l 3) '((l x = (if (> x 2) 4 5 )))) ---- >    4 
(fl-interp '(a 2)  '( (b X Y = (+ X Y))  (A W = (if (> W 1) 10 20 )) ))    --- > 10 
(fl-interp '(count ( 1 2  3) )   '( count L = ( if (null L) 0 (+ 1 (count (cdr L) ) ) ) ) ----> 3 
……   


|#

;brief description of 'fl-interp'

; while in eval process, then we evaluable a expression, if that function of that expression is not built-in,
; then use xassoc and replace 
; to find the binding and replace the binding into the context.  
; to replace the function name with function body,      (xassoc 'fname get-func-list (E) get-body-list (E) ).    
; and then as for example like,      f x y = (+ x y)    
;  if (E = " (f 3 4) ), then we (xreplace '((f (+ x y))) '(f 1 2)) --- >  ((+x y) 1 2)    x -> 1 y -> 2 
; binding and replacing the params in (+x y).     (eval (xreplace '((x 1)(y 2)) '(+ x y)))  --- > 3 
; how to do recursion, in this case, find the function application and its parameter in P,
; and we sub the func app with the body, then 
; binding the parameter with the arguments in that function body, it it is recursion function, 
;then simply we just check if we reach the base ;
; case, if yes, answer here we are, if not, then do another and binding variable. then done. 





;main program for EVAL. 

(defun fl-interp (E P) 
    (cond 
	   ((atom E ) E )                    ; base case for the recursion eval
             ((numberp E) E)  	         ; test if it is a number 
             ((null E) E) 		         ; test if it is a empty list 
             (t 
	        ; construct the function list, body list and argument list from P  
	        (let (( f (car E )) (arg (cdr E )) (f-list (get-func-list P)) (b-list (get-body-list P)) (arg-list (get-arg-list P)))   
                     (cond 
	                ; deal with built-in functions
                         ((eq f 'first) (car (fl-interp (car arg) p)))
                         ((eq f 'rest ) (cdr (fl-interp (car arg) p)))
                         ((eq f 'cons ) (cons (fl-interp (first arg) p) (fl-interp (second arg) p)))
                         ((eq f 'eq   ) (eq (fl-interp (first arg) p) (fl-interp (second arg) p )))
                         ((eq f 'atom ) (atom (fl-interp (first arg) p)))  
                         ((eq f 'equal) (equal (fl-interp (first arg) p) (fl-interp (second arg) p)))
                         ((eq f 'if   ) (if (fl-interp (car arg) p) (fl-interp (second arg) p) (fl-interp (third arg) p)))   
                         ((eq f 'cond ) (evcond (cdr E) P))
                         ((eq f 'null ) (null (fl-interp (car arg) p)))
                         ((eq f 'number) (numberp (fl-interp (car arg) p)))
                         ((eq f 'and  ) (and (fl-interp (first arg) p) (fl-interp (second arg) p)))
                         ((eq f 'or   ) (or (fl-interp (first arg) p) (fl-interp (second arg) p)))
                         ((eq f 'not  ) (not (fl-interp (car arg) p)))
                         ((eq f '+    ) (+ (fl-interp (first arg) p) (fl-interp (second arg) p)))
                         ((eq f '-    ) (- (fl-interp (first arg) p) (fl-interp (second arg) p)))
                         ((eq f '*    ) (* (fl-interp (first arg) p) (fl-interp (second arg) p)))
                         ((eq f '>    ) (> (fl-interp (first arg) p) (fl-interp (second arg) p)))
                         ((eq f '<    ) (< (fl-interp (first arg) p) (fl-interp (second arg) p)))
                         ((eq f '=    ) (= (fl-interp (first arg) p) (fl-interp (second arg) p)))
            
                        ; else it is the user defined functions         
                         ((func-exist f f-list) 
                             (let ((body (list (cons f (list (xassoc f f-list b-list))))) )          ;  body '(f (+ x y)) 
                                  (let ((context (xreplace body E)) )                	               ; ((+x y) 1 2)
                                       (let ((p-list (get-plist (cdr context)) ))                           ; ((1) (2)) 
                                                  (fl-interp (make-E context f f-list arg-list p-list) P)      ; 
                                        )
                                   )                                                                        
                              )   
                          )
		       ; evaluate list recursively 
   	                ((not (atom f)) (cons (fl-interp (car E) P) (fl-interp (cdr E) P)))     
		       ; other case return E since it is just simply a list.  
                         (t       E)) 	 
                     )
                )
        )  					  
)






;construct the format to apply xreplace to substitution. 
;produce the new function body after name binding.
;testcase:   
;(make-E '((+ x y) 1 2) 'f '((f)(g)) '(((x)(y))((w)(z))) '((1)(2)))---> (+ 1 2)
(defun make-E (ct fn f-list arg-list p-list)
       (xreplace 
	     (make-closure fn f-list arg-list p-list) 
       (car ct) ) 
)


;used to construct the argument and parameter list into the format required by xassoc;
;make-closure does thing to find the corrensponding arguments list by a given f name.
;testcase: (make-closure 'f '((f) (g)) '((+ x y)(+ w z)) '((1) (2)) )  ---- >  ((x 1) (y 2) )  
(defun make-closure (f f-list arg-list p-list)   
        (make-closure-helper
	      (get-plist 
		     (get-arg (xassoc f f-list arg-list))) 
   	p-list) 
)


;used to construct the argument and parameter in ((a1 p1) (a2 p2)). 
;testcase:   (make-closure-helper '((x)(y) '((1)(2)))----> ((x 1)(x 2)) 
(defun make-closure-helper (arg-list p-list)    
        (if (null arg-list)  nil
             (cons (cons (caar arg-list) (car p-list)) 
	         	 (make-closure-helper (cdr arg-list) (cdr p-list))
	     )
        )
)


; used to binding variable to value or function to body. 
; testcase:   (xreplace '((x 2) (y 3)) '(+ x y)) -----> (+ 2 3) 
(defun xreplace (s l)  
        (if (null s) l  
               (xreplace-ex (cadar s) (caar s) (xreplace (cdr s) l))
        ) 
)  

; used help binding the variable to value or function to body.  
; testcase:  (xreplace-ex 'he 'she '((she) is she)) ---> ((he) is he)
(defun xreplace-ex (s n l)  
       (cond ((null l) nil) 
             ((atom l) (if (eq n l) s l))
             (t 
	        (cons (xreplace-ex s n (car l)) 
			(xreplace-ex s n (cdr l))
                   )
  	     )
       )
)


;used to obtain the function body, l is P
;testcase:   (get-body-list '(  (g X = (g (g X)))   (h X = a )) ) ---->  ( (g (g X)))   ((h X = a )))
(defun get-body-list (l)  
       (mapcar 'get-body-list-helper l)
   )


; used to assist obtainning the body list, l is a program definition.
; testcase:  ;    (get-body-list '(  (g X = (g (g X)))   (h X = a )  ))    --->    (((G (G X))) (A))
(defun get-body-list-helper (l)  
       (cond 
            ((null l) nil)   
            ((eq '= (car l)) (cdr l))    
            (t (get-body-list-helper (cdr l))) 
       )
)


;used to obtain the argument list  
; test case:   (get-arg-list '(  (g X = (g (g X))) (h Y = a )  ))   ----->   ((X) (Y))
(defun get-arg-list(l)  
       (mapcar 'get-arg-list-good
               (mapcar 'cdr  
	              (mapcar 'get-arg-list-helper l)  ))   
 ) 


;cons the list when its length is greater than 2. 
;testcase:     (get-arg-list-good '(1 2 3)) ---->  ((1 2 3)) 
(defun get-arg-list-good(l)  
      (if (> (xcount l) 1) 
	      (list l)
	       l
	)
)


;helper function for obtainning arguments list 
;cons every thing after function name and before '= sigh. 
;testcase:   (get-arg-list-helper '(f x y = (+ x y)) ----- >  (x y) 
(defun get-arg-list-helper (l)  
       (cond 
                 ((null l) nil)   
                 ((eq '= (car l)) nil)    
                 (t (cons (car l) (get-arg-list-helper (cdr l)))))
)


;used to obtain the function list return nil if P is empty. 
;test case:  (get-func-list '(  (g X = (g (g X)))  (h X = a )  ))    ----- >   ((G) (H))
(defun get-func-list (l)  
           (mapcar 'get-func-list-helper l) 
)


;used to convert the function list into the the required  format for applying xassoc  
(defun get-func-list-helper (l)  
            (cons (car l) nil)
 )


;used to get the paramter list when evaluate a function 
; testcase:    (get-plist '(f 1 2) ) ---->  ((1) (2))
(defun get-plist (l)  
        (if (null l) 
             nil 
            (cons (list (car l)) (get-plist (cdr l) ))
         )
)    


;used to test if the function name is a user defined function 
;testcase:  (func-exist 'f '((f) (g)) ) ---- >   T
(defun func-exist (s l)  
     (xmember s (mapcar 'car l) ) 
)


; used to list the list if the list is atom 
; testcase:   (get-arg '(1 2)) ------>  ((1 2))  
(defun get-arg (l)
        (if (atom l) 
            (list l) 
             l
         )
)


;used to count number of elements in a flatten list
; testcase:   (xcount '(1 2 3)) ----->  3
(defun xcount (l) 
         (if (null l) 0 
             (+ 1 (xcount (cdr l)))
         )
)


;used to test if member recursively matter
;testcase:  (xxmember 's '(((s))) ) ---> T 
(defun xxmember (s l)
        (cond 
                   ((null l) nil)
                   ((atom l) (or (eq s l) (xmember s (cdr l))))
                  (t (or (xmember s (car l)) (xmember s (cdr l))))
          )
  )

;used to test if a member in flatten list 
;testcase:  (xmember 's '(s p l) ) ---> T 
(defun xmember (s l)  
       (if (null l) 
	   nil 
            (if (eq s (car l)) 
	        'T 
                 (xmember s (cdr l))
             )
        )
) 


; used to context and closure stuff 
; (xassoc 'f '((f)(g)) '((+ x y) (+ w z)) ) ------ >  (+ x y)   
(defun xassoc (x n v) 
       (if (null n) 
	   nil 
           (if (xmember x (car n )) 
               (xlocate x (car n) (car v)) 
               ( xassoc x (cdr n) (cdr v))
            )
        )
) 


; used as helper function of xassoc 
(defun xlocate (x l m )
      (if (eq x (car l)) 
	 (car m) 
          (xlocate x (cdr l) (cdr m))
      )
)


; used for condition cond 
(defun evcond (E P)   
      (if (fl-interp (car (car E)) P)     
          (fl-interp (second (car E)) P)    
          (evcond (cdr E) P )))











