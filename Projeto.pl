%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais

:- dynamic encomenda/5.
:- dynamic entrega/5.
:- dynamic estafeta/2.
:- dynamic cliente/3.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: definicoes iniciais

:- op( 900,xfy,'::' ).

solucoes(T,Q,S) :- findall(T,Q,S).
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
evolucao( Termo ) :-
  solucoes( Invariante,+Termo::Invariante,Lista ),
    insercao( Termo ), 
      teste( Lista ).

insercao( Termo) :-
        assert( Termo ).
insercao( Termo) :-
        retract( Termo ),!,fail.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

involucao( Termo ) :-
    solucoes( Invariante, -Termo::Invariante, Lista ),
      remocao( Termo ),
        teste( Lista ).

remocao( Termo ):-
        retract( Termo ).
remocao( Termo ):-
        assert( Termo ),!,fail.


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%----------------------- Base de Conhecimento  - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Encomenda
% Extensao do predicado encomenda : IdEncomenda, Freguesia, Peso, Volume, Preço -> { V, F }
encomenda(1,prado,2,5,50).
encomenda(2,maximinos,17,10,60).
encomenda(3,prado,12,67,250).
encomenda(4,lamacaes,77,18,27).
encomenda(5,gualtar,2,2,1500).
encomenda(6,lomar,1.5, 4, 30).
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Entrega
% Extensao do predicado entrega : IdEntrega, Data, IdEncomenda, Prazo, Transporte -> { V, F }
entrega(4,2/10/2021,2,13,bicicleta).
entrega(2,3/10/2021,3,1,moto).
entrega(1,4/11/2021,1,2,moto).
entrega(3,7/11/2021,6,3,carro).
entrega(5,7/11/2021,4,2,moto).
entrega(6,3/10/2021,5,7,bicicleta).
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Estafeta
% Extensao do predicado estafeta : NomeEstafeta, IdEntrega -> { V, F }
estafeta(manuel,1).
estafeta(luis,3).
estafeta(andre,2).
estafeta(maria,5).
estafeta(andre,4).
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Cliente
% Extensao do predicado cliente : NomeCliente, IdEntrega, Classificacao -> { V, F }
cliente(joao,1,3).
cliente(joaquim,5,3).
cliente(joao,2,2).
cliente(martim,3,5).
cliente(daniel,4,5).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%-----------------------------	PREDICADOS	- - - - -  - - - - -  -  -  
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Apresenta uma lista de todos os estafetas da empresa
% Extensão do predicado listarEstafetas: Lista -> {V,F}
listarEstafetas( L ) :- solucoes(Nome, estafeta(Nome, _), R), diferentes(R, L).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Apresenta uma lista de todos os clientes da empresa
% Extensão do predicado listarCliente: Lista -> {V,F}
listarCliente( L ) :- solucoes(Nome, cliente(Nome, _, _), R), diferentes(R, L).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Apresenta uma lista de todos as encomendas da empresa
% Extensão do predicado listarEncomendas: Lista -> {V,F}
listarEncomendas( L ) :- solucoes((IdEncomenda,Nome, Peso, Volume,  PrecoEncomenda), encomenda(IdEncomenda,Nome, Peso, Volume,  PrecoEncomenda), R), diferentes(R, L).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Apresenta uma lista de todos as entregas da empresa
% Extensão do predicado listarEntregas: Lista -> {V,F}
listarEntregas( L ) :- solucoes((IdEntrega, Data, IdEncomenda, Prazo, Transporte), entrega(IdEntrega, Data, IdEncomenda, Prazo, Transporte), R), diferentes(R, L).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- QUERY 1 - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

estafetaEcologico(FINAL) :- listarEstafetas(R), estafetaEcologicoAux(R,100,L,FINAL).

estafetaEcologicoAux([],_,L,L).
estafetaEcologicoAux([R|T],MIN,L,FINAL) :- calcula(R, R1),
				     (R1 < MIN) -> estafetaEcologicoAux(T, R1, R,FINAL) ; estafetaEcologicoAux(T, MIN, L, FINAL).

calcula(R, L) :- solucoes(Id, estafeta(R, Id), R1), devolveListaVeiculos(R1, R2), calculaValorEcologico(R2, L).
					 
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- QUERY 2 - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Apresenta uma lista dos estafetas que entregaram uma encomenda a um determinado cliente
% Extensão do predicado estafetaCliente: Nome cliente, Lista -> {V,F}

estafetaCliente(NomeCliente, L) :- solucoes(ID, cliente(NomeCliente, ID, Classificacao), S), diferentes(S, S1),
							   estafetaClienteAux(S1, L).

estafetaClienteAux([], []).							
estafetaClienteAux([ID|T] , L) :- solucoes(Nome, estafeta(Nome, ID), R1), estafetaClienteAux(T, R2), concatenar(R1, R2, L).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- QUERY 3 - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Apresenta uma lista dos clientes servidos por um determinado estafeta
% Extensão do predicado estafetaCliente: Nome cliente, Lista -> {V,F}

clienteEstafeta(NomeEstafeta, L) :- solucoes(ID, estafeta(NomeEstafeta, ID), S), diferentes(S, S1),
								clienteEstafetaAux(S1, L).

clienteEstafetaAux([],[]).
clienteEstafetaAux([ID|T], L) :- solucoes(Nome, cliente(Nome,ID, Classificacao), R1), clienteEstafetaAux(T, R2), concatenar(R1,R2,S), diferentes(S,L).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- QUERY 4 - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Apresenta a faturação da empresa num determinado dia
% Extensão do predicado estafetaCliente: Data, Variável -> {V,F}
fatura(Data, L) :- solucoes(IdEncomenda, entrega(IdEntrega, Data, IdEncomenda, Prazo, Transporte), S), diferentes(S, S1),
				 faturaAux(S1, S2), somaC(S2,L).

faturaAux([],[]).
faturaAux([ID|T], L) :- solucoes(Preco, encomenda(ID, Nome, Peso, Volume, Preco), R1),faturaAux(T,R2), concatenar(R1,R2, L).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- QUERY 5 - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

freguesiaPlus(Final) :- solucoes(Freguesia, encomenda(IdEncomenda, Freguesia, Peso, Volume, Preco), R), freguesiaPlusAux(R,0, L, Final).

freguesiaPlusAux([], _, L, L).     
freguesiaPlusAux([R|T],N, L, Final) :-  count(R, [R|T], N1),
                                 N1 > N -> removeAll(R, [R|T], S),  freguesiaPlusAux(S, N1, R, Final) 
                                 ; removeAll(R, [R|T], S), freguesiaPlusAux(S, N, L, Final).

removeAll(_, [], []).
removeAll(X, [X|T], L):- removeAll(X, T, L), !.
removeAll(X, [H|T], [H|L]):- removeAll(X, T, L ).

count(_, [], 0).
count(X, [X | T], N) :- !, count(X, T, N1), N is N1 + 1.
count(X, [_ | T], N) :- count(X, T, N).
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- QUERY 6 - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Apresenta a classificacao media de satisfação de um estafeta
% Extensão do predicado satisfacao: Nome Estafeta, Variável -> {V,F}
satisfacao(Nome, L) :- solucoes(ID, estafeta(Nome, ID), S), diferentes(S, S1),
						satisfacaoAux(S1, L1), media(L1, L).

satisfacaoAux([],[]).
satisfacaoAux([ID|T], L) :- solucoes(Classificacao, cliente(NomeCliente, ID, Classificacao), S), satisfacaoAux(T, S1), concatenar(S,S1, L).
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- QUERY 7 - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- QUERY 8 - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- QUERY 9 - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- QUERY 10 - - - - - -  -  -  -  -  -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

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

naoNegativoLista([]).
naoNegativoLista([H|T]) :- H >=0, naoNegativoLista(T).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Verifica se todos os elementos da lista são não negativos
% Extensao do predicado naoNegativo: L -> {V,F}  

naoNegativo(L) :- L >=0.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Verifica se contem um elemento numa dado lista
% Extensão do predicado contem: H,[H|T] -> {V, F}

contem(H, [H|T]).
contem(X, [H|T]) :- contem(X, T).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Faz a media de uma lista
% Extensao do predicado media: L,R -> {V,F}

media([H|T],S) :- somaC([H|T],S1), comprimento([H|T],S2), S is S1/S2.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

calculaValorEcologico([T], L) :- valorEcologico(T, L).
calculaValorEcologico([H|T], L) :- valorEcologico(H,R), calculaValorEcologico(T,RL), L is R+RL.

devolveListaVeiculos([], []).
devolveListaVeiculos([H|T], L) :- solucoes(Veiculo, entrega(H, _, _, _, Veiculo), R), devolveListaVeiculos(T,R1), concatenar(R,R1,L).

valorEcologico(bicicleta, L):- L is 1.
valorEcologico(moto, L):- L is 2.
valorEcologico(carro, L):- L is 3.


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%----------------------------- Invariantes - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- Encomenda - - - - -  -  -  -  -  -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% IdEncomenda, Freguesia, Peso, Volume, Preço
+encomenda(IdEncomenda,_,_,_,_) :: (solucoes((IdEncomenda), (encomenda(IdEncomenda,_,_,_,_)), R), 
                                   comprimento(R, L), L == 1).

+encomenda(_,_,Peso,_,_) :: (solucoes((Peso), (encomenda(_,_,Peso,_,_)), R), 
                            naoNegativoLista(R)).

+encomenda(_,_,_,Volume,_) :: (solucoes((Volume), (encomenda(_,_,_,Volume,_)), R), 
                            naoNegativoLista(R)).

+encomenda(_,_,_,_,Preco) :: (solucoes((Preco), (encomenda(_,_,_,_,Preco)), R), 
                            naoNegativoLista(R)).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- Entrega -  - - - - -  -  -  -  -  -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% IdEntrega, Data, IdEncomenda, Prazo, Transporte

+entrega(IdEntrega,_,_,_,_) :: (solucoes(IdEntrega), (encomenda(IdEntrega,_,_,_,_)), R),
                                comprimento(R, L), L == 1).

+entrega(_,Data,_,_,_) :: 

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- Estafeta - - - - - -  -  -  -  -  -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- Cliente  - - - - - -  -  -  -  -  -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -