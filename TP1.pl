% potencia/2(+C,-P)
potencia([],[]).
potencia([X|Xr],P):-
	subtract([X|Xr],[X],R),
	potencia(R,S),
	agregar([X],S,O),
	union(O,S,P).

%agregar/3(E,C,F)
agregar(E,[],[E,[]]).
agregar(E,[C|Cr],F):-
	agregar(E,Cr,M),
	append(E,C,P),
	union(M,[P],F).
   
   
% cartesiano/2(+A,+B,-C)
 cartesiano([],_,[]).
 cartesiano(_,[],[]).
 cartesiano([],[],[]).
 cartesiano([X|Xr],[Y|Yr],C):-
 	cartesiano(Xr,[Y|Yr],R2),
	cartesiano([X],Yr,R1),
	append(R1,R2,R3),
	append([[X,Y]],R3,C).
cartesiano(_,_,[]):-write('error: al menos uno de los argumentos no es un conjunto').

