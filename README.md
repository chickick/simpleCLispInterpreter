# fl-interpret simpleCLispInterpreter

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
