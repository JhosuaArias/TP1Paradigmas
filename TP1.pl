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
	
%encripta/5(+He,+Ae,+As,-Hs,-Ef) He: hilera a encriptar, Ae: alfabeto de entrada, As: alfabeto de salida
%Ef: estado final de la maquina formado por un par [ae,as], donde ae y as son simbolos del alfabeto de 
%entrada y salida en lo que quedo la maquina luego de encriptar He. Hs: hilera encriptada.
encripta(He,_,_,_,_):- not(is_list(He)), write('error: alguno de los argumentos no es válido'),!, fail.
encripta(_,Ae,_,_,_):- not(is_list(Ae)), write('error: alguno de los argumentos no es válido'),!, fail.
encripta(_,_,As,_,_):- not(is_list(As)), write('error: alguno de los argumentos no es válido'),!, fail.	
	
	
encripta(He,Ae,As,Hs,Ef):-
	crea_alfabeto_entrada(Ae),
	crea_alfabeto_salida(As),
	encripta_rec(He,Ae,As,Hs,Ef),
	retractall(alfabetoE(X)),
	retractall(alfabetoS(X)).
	

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

decripta(Hs,Ae,As,Ef,He):-
	crea_alfabeto_entrada(Ae),
	crea_alfabeto_salida(As),
	decripta_rec(Hs,Ae,As,Ef,He),
	retractall(alfabetoE(X)),
	retractall(alfabetoS(X)).

decripta_rec(Hs,Ae,As,Ef,He).
	
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