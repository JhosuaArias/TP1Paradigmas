%bpp/3(+N,+A,-S) Devuelve el subárbol de A cuya raiz es N
bpp(N,_,_):- is_list(N), write('Error, el primer elemento ingresado es una lista'),!,fail.

bpp(N,[N|Yr],[N|Yr]):-
	write(N).
	
bpp(N,[X|Y],S):-
	write(X),
	qr(N,Y,S).

%qr/3(+N,+A,-Yr) Devuelve el subarbol de A cuya raiz es N, revisando las ramas del A para ver si alguna es N
qr(N,[A|Ar],Yr):-
	is_list(A),
	bpp(N,A,Yr).
	
qr(N,[N|_],Yr):-
	write(N),!.
	
qr(N,[A|Ar],Yr):-
	atom(A),write(A),qr(N,Ar,Yr).
	
qr(N,[A|Ar],Yr):-
	is_list(A),qr(N,Ar,Yr).	