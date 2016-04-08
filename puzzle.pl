/*
STUDENT NAME:       JINXIN HE
STUDENT NUMBER:     1265977
COURSE  NUMBER:     CMPUT 325
SECTION  NUMBER:    LEC A1
ASSIGNMENT NUMBER:  #4
*/



/* --------------------------------------------------------------------------------------
part 1 
 validPuzzle(N,M,Pieces) which just check the validness of the puzzle input. 
 N is the number of column, M is the number of rows, Pieces are N*M single pieces.  
 
 helper-predicates: 
    xmember(X,L)        :  check if X is Member of a list 
    validC(D1,D2,D3,D4) : check if a group has valid color
    size(L,N)           : check if L has length N and also check the validness of color
    checkBounds(N,M,Pieces) : check if Pieces has valid number of boundary pieces. 
    
    
---------------------------------------------------------------------------------------*/ 

validColor(boundary).
validColor(red).
validColor(blue).
validColor(green).
validColor(yellow).

 
xmember(X,[X|_]). 
xmember(X,[_|T]) :- xmember(X,T). 


validC(D1,D2,D3,D4) :- 
              validColor(D1), validColor(D2), 
              validColor(D3), validColor(D4). 
                          
size(List, N) :-  size(List, 0, N).
size([], N, N).      
size([H|T], L, N) :- H = p(Top,Bottom,Left,Right),  
 					 validC(Top,Bottom,Left,Right),   
                     L1 is L + 1, size(T, L1, N).  
                     
                     
% check for specified number of boundary. 
v1Boundary([],0). 
v1Boundary([H|T],L) :- H = p(boundary,_,_,_), v1Boundary(T,LL), L is LL + 1,!. 
v1Boundary([_|T],L) :- v1Boundary(T,L).  

v2Boundary([],0). 
v2Boundary([H|T],L) :- H = p(_,boundary,_,_), v2Boundary(T,LL), L is LL + 1,!. 
v2Boundary([_|T],L) :- v2Boundary(T,L). 

v3Boundary([],0). 
v3Boundary([H|T],L) :- H = p(_,_,boundary,_), v3Boundary(T,LL), L is LL + 1,!. 
v3Boundary([_|T],L) :- v3Boundary(T,L). 

v4Boundary([],0). 
v4Boundary([H|T],L) :- H = p(_,_,_,boundary), v4Boundary(T,LL), L is LL + 1,!. 
v4Boundary([_|T],L) :- v4Boundary(T,L). 

v5Boundary([],0). 
v5Boundary([H|T],L) :- H = p(boundary,_,boundary,_), v5Boundary(T,LL), L is LL + 1,!. 
v5Boundary([_|T],L) :- v5Boundary(T,L). 

v6Boundary([],0). 
v6Boundary([H|T],L) :- H = p(boundary,_,_,boundary), v6oundary(T,LL), L is LL + 1,!. 
v6Boundary([_|T],L) :- v6Boundary(T,L). 

v7Boundary([],0). 
v7Boundary([H|T],L) :- H = p(_,boundary,boundary,_), v7Boundary(T,LL), L is LL + 1,!. 
v7Boundary([_|T],L) :- v7Boundary(T,L). 

v8Boundary([],0). 
v8Boundary([H|T],L) :- H = p(_,boundary,_,boundary), v8Boundary(T,LL), L is LL + 1,!. 
v8Boundary([_|T],L) :- v8Boundary(T,L). 


checkBounds(N,M,Pieces) :-  
        v1Boundary(Pieces,L1), L1 == N, 
		v2Boundary(Pieces,L2), L2 == N,
		v3Boundary(Pieces,L3), L3 == M, 
		v4Boundary(Pieces,L4), L4 == M,
        v5Boundary(Pieces,L5), L5 == 1, 
        v6Boundary(Pieces,L6), L6 == 1, 
        v7Boundary(Pieces,L7), L7 == 1, 
        v8Boundary(Pieces,L8), L8 == 1. 
        

                    
validPuzzle(N,M,Pieces) :- 
	 N > 1, M > 1,          
	 S is N * M, 		    
	 checkBounds(N,M,Pieces),
	 size(Pieces, S).        
	 
	 
                     

/* --------------------------------------------------------------------------------------
part 2
  horizontalNeighbors(L,R) which just check the valid neighborship of two input pieces. 
  L is the piece on the left hand side of pieces R. 
 
 helper-predicates:  
   vc(C)    : check if C is a valid color without considering boundary
   vbound(p1,p2)  : check if p1 is a left neighbor of p2 by listing all cases.
 
---------------------------------------------------------------------------------------*/              
                     
                     
                     
vc(yellow).
vc(red).
vc(blue).
vc(green).
vcg(D1,D2,D3,D4) :- vc(D1),vc(D2), vc(D3), vc(D4). 



% L at top left corner, R at top or right top corner when size is 2

vbound(p(boundary,D1,boundary,D2),p(boundary,E1,E2,E3)) :- 
           vc(D1), vc(D2), vc(E1), vc(E2), validColor(E3). 
                   
           
% L at bottom left, R at bottom, or bottom right corner when size is 2,   
vbound(p(D1,boundary,boundary,D2),p(E1,boundary,E2,E3)) :- 
           vc(D1), vc(D2), vc(E1), vc(E2), validColor(E3).  
             

% L at top middle, R top middle or right top corner 
vbound(p(boundary,D1,D2,D3),p(boundary,E1,E2,E3)) :- 
           vc(D1), vc(D2), vc(D3), vc(E1), vc(E2), validColor(E3). 
                 

% L at bottom middle, R bottom middle or bottom right corner
vbound(p(D1,boundary,D2,D3),p(E1,boundary,E2,E3)) :- 
           vc(D1), vc(D2), vc(D3), vc(E1), vc(E2), validColor(E3).  
           
  
% L at left middle, R right to L  
vbound(p(D1,D2,boundary,D3),p(E1,E2,E3,E4)) :- 
           vc(D1), vc(D2), vc(D3), vc(E1), vc(E2), vc(E3), validColor(E4).
            
           
% L at middle somewhere, R right to L or right bound           
vbound(p(D1,D2,D3,D4),p(E1,E2,E3,E4)) :- 
           vcg(D1,D2,D3,D4), vc(E1), vc(E2), vc(E3), validColor(E4).  
                      
 
                
 
                     
horizontalNeighbor(p(LT,LB,LL,LR),p(RT,RB,RL,RR)) :-   
 	         vbound(p(LT,LB,LL,LR),p(RT,RB,RL,RR)), 
             LR == RL.
             
             
             



/* --------------------------------------------------------------------------------------
part 3
  validSolution(N,M,Pieces,Solution) checks if Solution is valid defined by N,M and Pieces

  helper-predicates:  
     flatten(M,L)   :  flatten a list M, and L is the flattened list 
     validRow(P)    :  check if P is a valid row of solution.
     minis(L,R,O)   :  O is the difference of list L and R. 
    
---------------------------------------------------------------------------------------*/  
        
      

flatten([],[]).
flatten([Head|InTail], [Head|OutTail]) :-
	Head \= [],
	Head \= [_|_],
    flatten(InTail,OutTail).

%% For a non-empty list: flatten the head and flatten the tail and append result
flatten([Head|InTail],Out) :-
	flatten(Head,FlatHead),
	flatten(InTail,OutTail),
	append(FlatHead,OutTail,Out).   
	
	
                             
minusAcc(L,[],_,L) :- !.
minusAcc([],_,A,A) :- !.
minusAcc([H|T],SL,A,W) :- \+memberchk(H,SL),!,
                            append([H],A,AL),
                            minusAcc(T,SL,AL,W).
                            
minusAcc([_|T],SL,A,W) :- minusAcc(T,SL,A,W).
minus(L,SL,W) :- minusAcc(L,SL,[],W).                    
                     
                     
                    
validRow([H|[]]). 
validRow([A,B|T]) :- horizontalNeighbor(A,B), validRow([B|T]).

validRows([]).
validRows([H|T]) :- validRow(H), validRows(T). 





validSolution(N, M, Pieces, Solution) :-   
             flatten(Solution,Sol), distinct(Sol,Sol), 
             distinct(Pieces,Pieces), minus(Sol, Pieces,[]), validRows(Solution).
             
             
             
             
             
             
/* --------------------------------------------------------------------------------------
part 4
  jigsaw(N,M,Pieces,Solution) generate jigsaw solution.
  

---------------------------------------------------------------------------------------*/          
             
             
             
        
        
  %----------------- fact for vertical neighbors ---------------
    % L top, R bottom, vertical neighbor 
    % L at top left corner, R at left down by L or bottom left when size is 2.  
    % orL at left bound in middle, R left bound down by L, or left bottom corner. 
vvbound(p(D3,D1,boundary,D2),p(E1,E3,boundary,E2)) :- 
           vc(D1), vc(D2), vc(E1), vc(E2), validColor(E3), validColor(D3).   
                

    % L at top right, R at right down by L or bottom right when size is 2.
    % L at right bound middle somewhere, R down by L, or bottom right corner.
vvbound(p(D3,D1,D2,boundary),p(E1,E2,E3,boundary)) :- 
           vc(D1), vc(D2), vc(E1), vc(E3), validColor(D3), validColor(E2).              
                 
    % L at top middle somewhere, R is down by it or the bottom bound when size 2.
    % L at middle somewhere, R down by L or on the bottom bound. 
vvbound(p(D4,D1,D2,D3),p(E1,E2,E3,E4)) :- 
           vc(D1), vc(D2), vc(D3), vc(E1), vc(E3),vc(E4), validColor(E2), validColor(D4).   
           
    %-------------------------------------------------------------            
    
      
      
% using accumulator to store the list without the 'N' one 
findPieces([Head|Tail],Rest,N,NewList) :- N == Head, append(Tail,Rest, NewList). 
findPieces([Head|Tail],Rest,N,Newlist) :- findPieces(Tail,[Head|Rest], N, NewList).  



generateTopRow(1,Pieces,FirstPieceRow,RemainPiece,OutputRow).
generateTopRow(N,Pieces,[H|Rest], NewPieces,OutputRow) :- 
      N > 1,  
      ((N = 2) -> (horizontalNeighbor(p(boundary,Bottom, boundary, Right), H), 
      findPieces( Pieces,[],p(boundary,Bottom,boundary,Right),NewPieces), 
      N1 is N -1, 
      generateTopRow(N1, NewPieces,[p(boundary, Bottom, boundary, Right), H|Rest],
      NewPieces,OutputRow));  
     ( horizontalNeighbor(M,H),findPieces(Pieces,[], NewPieces,OutputRow ), 
      N1 is N - 1, 
      generateTopRow(N1, NewPieces, [L,R|Rest], NewPieces, OutputRow))).  



 
          
             
             
             
             
jigsaw(N,M,Pieces,[Output|Solution]) :- 
            findPieces(Pieces,[],p(boundary,Bottom,Left,boundary), NewPieces),
  			generateTopRow(N,NewPieces,[p(boundary,Bottom, Left,boundary)], Var,Output),
  			M1 is M - 1,
  			generateOtherRows(N,M1,Output,Var,Solution). 
             
             
             
             
             
             
             
             
             
                                   
