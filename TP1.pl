%Tarea Programada 1 Paradigmas Computacionales
% Jhosua Arias - B50657
% Pablo Madrigal - B33925
% Jorge Remón - B55825

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
   
   
%cartesiano/2(+A,+B,-C) Devuelve una lista a la que se le aplicó la operación cartesiano
cartesiano(X,_,_) :- not(is_list(X)), write('error: al menos uno de los argumentos no es un conjunto'), !, fail.
cartesiano(_,X,_) :- not(is_list(X)), write('error: al menos uno de los argumentos no es un conjunto'), !, fail.
cartesiano([],_,[]) :- !.
cartesiano(_,[],[]) :- !.
cartesiano([X|Xr],[Y|Yr],C):-
 	cartesiano(Xr,[Y|Yr],R2),
	cartesiano([X],Yr,R1),
	append(R1,R2,R3),
	append([[X,Y]],R3,C).
	
%bpp/3(+N,+A,-S) Devuelve el subárbol de A cuya raiz es N
bpp(N,_,_):- is_list(N), write('Error, el primer elemento ingresado es una lista'),!,fail.

bpp(N,[N|Yr],[N|Yr]):-
	write(N), !.
bpp(N,[X|Y],S):-
	write(X),
	qr(N,Y,S).
bpp(_,[],[]):- !.
	
%qr/3(+N,+A,-Yr) Devuelve el subarbol de A cuya raiz es N, revisando las ramas del A para ver si alguna es N
qr(N,[A|_],Yr):-
	is_list(A),
	bpp(N,A,Yr).
qr(N,[N|_],N):-
	write(N),!.
qr(N,[A|Ar],Yr):-
	atom(A),write(A),qr(N,Ar,Yr).
qr(N,[A|Ar],Yr):-
	is_list(A),qr(N,Ar,Yr).
	

%bap(+N,+A,-S)/2 S es el subarbol de A cuya raiz es N, nil si el subarbol no existe
	
bap(N,_,_):- 
	is_list(N), !, fail.
	
bap(N,[],[]):- !.

bap(N,[N|Nr],N):-
	write(N), !. 
	
bap(N,[[N|Ar]|Arr],[N|Ar]):- 
	write(N), !.

bap(N,[A|Ar],S):-
	atom(A),
	write(A), write(" "),
	bap(N,Ar,S).

bap(N,[[A|Ar]|Arr],S):-
	write(A), write(" "),
	append(Arr,Ar,L),
	bap(N,L,S).
	
%encripta/5(+He,+Ae,+As,-Hs,-Ef) He: hilera a encriptar, Ae: alfabeto de entrada, As: alfabeto de salida
%Ef: estado final de la maquina formado por un par [ae,as], donde ae y as son simbolos del alfabeto de 
%entrada y salida en lo que quedo la maquina luego de encriptar He. Hs: hilera encriptada.
encripta(He,_,_,_,_):- not(is_list(He)), write('error: alguno de los argumentos no es válido'),!, fail.
encripta(_,Ae,_,_,_):- not(is_list(Ae)), write('error: alguno de los argumentos no es válido'),!, fail.
encripta(_,_,As,_,_):- not(is_list(As)), write('error: alguno de los argumentos no es válido'),!, fail.	
	
encripta(He,Ae,[As|Asr],Hs,Ef):-
	crea_alfabeto_entrada(Ae),
	reversa(Asr,RAsr),
	append([As],RAsr,RAs),
	crea_alfabeto_salida(RAs),
	encripta_rec(He,Ae,RAs,Hs,Ef),
	retractall(alfabetoE(X)),
	retractall(alfabetoS(X)).
	
%encripta_rec/5(He,Ae,As,Hs,Ef) es la parte recursiva de encripta/5.
encripta_rec([],[He|_],[As|_],[],[He,As]):- !.
encripta_rec([He|Her],[He|Aer],[As|Asr],Hs,Efs):-
	encripta_rec(Her,[He|Aer],[As|Asr],Xr,Efs),
	append([As],Xr,Hs),
	!.
encripta_rec([He|Her],_,_,Hs,Efs):-
	rota_alfabeto_entrada(P),
	rota_alfabeto_salida(Q),
	encripta_rec([He|Her],P,Q,Hs,Efs).
	
	
%decripta/5(+Hs,+Ae,+As,+Ef,-He) Hs: hilera encriptada, Ae: alfabeto de entrada, As: alfabeto de salida,
%Ef: estado final de la maquina formado por un par [ae,as], donde ae y as son simbolos del alfabeto de 
%entrada y salida en lo que quedo la maquina luego de encriptar He. He: hilera desencriptada.
decripta(Hs,_,_,_,_):- not(is_list(Hs)),write('error: alguno de los argumentos no es válido'),!, fail.
decripta(_,Ae,_,_,_):- not(is_list(Ae)),write('error: alguno de los argumentos no es válido'),!, fail.
decripta(_,_,As,_,_):- not(is_list(As)),write('error: alguno de los argumentos no es válido'),!, fail.
decripta(_,_,_,Ef,_):- not(is_list(Ef)),write('error: alguno de los argumentos no es válido'),!, fail.

decripta(Hs,Ae,[As|Asr],_,He):-
	crea_alfabeto_salida(Ae),
	reversa(Asr,RAsr),
	append([As],RAsr,RAs),
	crea_alfabeto_entrada(RAs),
	decripta_rec(Hs,RAs,Ae,He),
	retractall(alfabetoE(X)),
	retractall(alfabetoS(X)).
	
%decripta_rec/5(Hs,Ae,As,Ef,He) es la parte recursiva de decripta/5.
decripta_rec([],_,_,[]):- !.
decripta_rec([He|Her],[He|Aer],[As|Asr],Hs):-
	decripta_rec(Her,[He|Aer],[As|Asr],Xr),
	append([As],Xr,Hs),
	!.
decripta_rec([He|Her],_,_,Hs):-
	rota_alfabeto_entrada(P),
	rota_alfabeto_salida(Q),
	decripta_rec([He|Her],P,Q,Hs).
	
%crea_alfabeto_entrada/1(+Ae) crea un hecho para que Ae sea un alfabeto de entrada
crea_alfabeto_entrada(Ae):- assert(alfabetoE(Ae)).
%crea_alfabeto_salida/1(As) crea un hecho para que As sea un alfabeto de salida
crea_alfabeto_salida(As):- assert(alfabetoS(As)).

%rota_alfabeto_entrada/1(L) rota el alfabeto de entrada un diente
rota_alfabeto_entrada(L):- 
	retract(alfabetoE([Cabeza|Resto])),
	append(Resto,[Cabeza],L),
	assert(alfabetoE(L)).
%rota_alfabeto_salida/1(L) rota el alfabeto de salida un diente
rota_alfabeto_salida(L):-	
	retract(alfabetoS([Cabeza|Resto])),
	append(Resto,[Cabeza],L),
	assert(alfabetoS(L)).

% reversa(+Lista,-Reversa): Reversa es la lista Lista invertida
%    Lista, Reversa: listas
%    ?- reversa([a,b],X). -> X = [b,a]
%    ?- reversa([a,b,[c,d],e],X). -> X = [e,[c,d],b,a]
reversa([X],[X]):- !.
reversa([X|M],Z):-
	reversa(M,Y),
	append(Y,[X],Z).