%potencia/2(+C,-P) Devuelve una lista a la que se le aplicó la operación potencia 
potencia(X,_):- not(is_list(X)), write('error: el argumento no es un conjunto'), !, fail.
potencia([],[]) :- !.
potencia([X|Xr],P):-
	subtract([X|Xr],[X],R),
	potencia(R,S),
	agregar([X],S,O),
	union(O,S,P).

%agregar/3(+E,+C,-F) devuelve una lista de listas, en donde a cada lista se le agregó el elemento E
agregar(E,[],[E,[]]):- !.
agregar(E,[C|Cr],F):-
	agregar(E,Cr,M),
	append(E,C,P),
	union(M,[P],F).
   
   
% cartesiano/2(+A,+B,-C) Devuelve una lista a la que se le aplicó la operación cartesiano
cartesiano(X,_,_) :- not(is_list(X)), write('error: al menos uno de los argumentos no es un conjunto'), !, fail.
cartesiano(_,X,_) :- not(is_list(X)), write('error: al menos uno de los argumentos no es un conjunto'), !, fail.
cartesiano([],_,[]) :- !.
cartesiano(_,[],[]) :- !.
cartesiano([X|Xr],[Y|Yr],C):-
 	cartesiano(Xr,[Y|Yr],R2),
	cartesiano([X],Yr,R1),
	append(R1,R2,R3),
	append([[X,Y]],R3,C).
