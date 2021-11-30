

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).					

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: definicoes iniciais

:- op( 900,xfy,'::' ).


solucoes(T,Q,S) :- findall(T,Q,S).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -

evolucao( Termo ) :-
	solucoes(Invariante,+Termo::Invariante,Lista),
	insercao(Termo),
	teste(Lista).

insercao( Termo ) :- assert( Termo ).
insercao( Termo ) :- retract( Termo ),!,fail.

teste([]).
teste( [H|T] ) :- H, teste( T ).

comprimento( [],0 ).
comprimento( [H|T],R ) :- comprimento( T,N ), R is N+1.





%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -


% -> transporte(peso maximo, velocidade media com esse peso)
bicicleta(5,10).      
mota(20,35).            
carro(100, 25). 

% -> entrega(id, nome, freguesia, peso (Kg), volume (Cm3), prazo(dias))
entrega(1, brinquedo, gualtar, 10, 5, 3).
entrega(2, carta, lamacaes, 0.5, 1, 2).
entrega(3, roupa, prado, 2, 3, 7).
entrega(4, bilhetes, vila verde, 0.4, 1.1, 4).
entrega(5, computador, lamacaes, 3.5, 2.2, 6).
entrega(6, sapatos, maximinos, 1.4, 3.2, 5).

% -> estafeta(nome, id, lista de entregas, meio transporte).
estafeta(joao, 1, [entrega(1, brinquedo, gualtar, 10, 5, 3), entrega(2, carta, lamacaes, 0.5, 1, 2)], mota(20,35)).
estafeta(jose, 2, [entrega(3, roupa, prado, 2, 3, 7),entrega(5, computador, lamacaes, 3.5, 2.2, 6)],entrega(4, bilhetes, vila verde, 0.4, 1.1, 4), carro(100,25)).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%---------------------- Predicados Auxiliares  - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% remove os elementos repetidos de uma lista
% Extensao do predicado diferentes: L1, L2 -> {V,F}

diferentes( [],[] ).
diferentes( [X|L],[X|NL] ) :- removerElemento( L,X,TL ), diferentes( TL,NL ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% remove um elemento de uma lista
% Extensao do predicado removerElemento: L1, Y, L2 -> {V,F}

removerElemento( [],_,[] ).
removerElemento( [X|L],X,NL ) :- removerElemento( L,X,NL ).
removerElemento( [X|L],Y,[X|NL] ) :- X \== Y, removerElemento( L,Y,NL ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Soma os elementos de uma lista
% Extensao do predicado somaC : LN,R -> {V,F}

somaC([X],X).
somaC([X|L],R):- somaC(L,RL), R is X+RL.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% concatena duas listas
% Extensao do predicado concatenar: L1,L2,L3 -> {V,F}  

concatenar( [],L,L ).
concatenar( [H|T],L2,[H|L] ) :- concatenar(T,L2,L).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Calcula o comprimento de uma lista
% Extensao do predicado comprimento: L,R -> {V,F}       

comprimento([],0).
comprimento([H|T],R) :- comprimento(T,N), R is N+1.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Testa todos os elementos da lista
% Extensão do predicado teste: [R|LR] -> {V,F}

teste([]).
teste([I|L]) :- I, teste(L).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Verifica se todos os elementos da lista são não negativos
% Extensao do predicado naoNegativo: L -> {V,F}  

naoNegativo([]).
naoNegativo([H|T]) :- H>=0, naoNegativo(T).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Verifica se contem um elemento numa dado lista
% Extensão do predicado contem: H,[H|T] -> {V, F}

contem(H, [H|T]).
contem(X, [H|T]) :- contem(X, T).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Faz a media de uma lista
% Extensao do predicado media: L,R -> {V,F}

media([H|T],S) :- somaC([H|T],S1), comprimento([H|T],S2), S is S1/S2.