%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais

:- dynamic encomenda/6.
:- dynamic entrega/5.
:- dynamic estafeta/2.
:- dynamic cliente/3.
:- dynamic concluido/3.

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
% Concluido - Predicado para dar as encomendas que já foram entregues
% Extensao do predicado concluido : IdEntrega, IdEncomenda, DataConcluida -> { V, F }
concluido(5, 4, date(2021,11,8)).
concluido(7, 7, date(2021,6,9)).
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Encomenda
% Extensao do predicado encomenda : IdEncomenda, Freguesia, Peso, Volume, Preço, Estado -> { V, F }
encomenda(1, prado,     15,   5,  50,   pendente).
encomenda(2, maximinos, 3,  10, 60,   caminho).
encomenda(3, prado,     12,  67, 250,  pendente).             
encomenda(4, lamacaes,  10,  18, 27,   finalizada).
encomenda(5, gualtar,   2,   2,  1500, pendente).
encomenda(6, lomar,     70, 4,  30,   caminho).
encomenda(7, prado,     30,   4,  42,   finalizada).
encomenda(8, merelim,   35,   3,  25,   caminho).
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Entrega       
% Extensao do predicado entrega : IdEntrega, Data, IdEncomenda, Prazo, Transporte -> { V, F }
entrega( 4, date(2021,10,2), 2,13, bicicleta).
entrega( 2, date(2021,10,3), 3, 1, moto).
entrega( 1, date(2021,11,4), 1, 2, moto).
entrega( 3, date(2021,11,7), 6, 3, carro). 
entrega( 5, date(2021,11,7), 4, 2, moto).
entrega( 6, date(2021,10,3), 5, 7, bicicleta).
entrega( 7, date(2021,6 ,4), 7, 8, carro).
entrega( 8, date(2021,9,10), 8, 5, carro).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Estafeta
% Extensao do predicado estafeta : NomeEstafeta, IdEntrega -> { V, F }
estafeta(manuel,1).
estafeta(luis,3).
estafeta(andre,2).
estafeta(maria,5).
estafeta(andre,4).
estafeta(maria,7).
estafeta(andre,6).
estafeta(luis,8).
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Cliente
% Extensao do predicado cliente : NomeCliente, IdEntrega, Classificacao -> { V, F }
cliente(joao, 1,3).
cliente(joaquim, 5,3).
cliente(joao, 2,2).
cliente(martim, 3,5).
cliente(daniel, 4,5).
cliente(ze, 8, 4).
cliente(joao,6, 3.5).
cliente(martim, 7, 2).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%-----------------------------	PREDICADOS  - - - - -  - - - - -  -  -  
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

% Apresenta uma lista de todos os estafetas da empresa
% Extensão do predicado listarEstafetas: Lista -> {V,F}
listarEstafetas( L ) :- solucoes(Nome, estafeta(Nome, _), R), diferentes(R, L).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Apresenta uma lista de todos os clientes da empresa
% Extensão do predicado listarCliente: Lista -> {V,F}
listarCliente( L ) :- solucoes(Nome, cliente(Nome, _, _), R), 
                      diferentes(R, L).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Apresenta uma lista de todos as encomendas da empresa
% Extensão do predicado listarEncomendas: Lista -> {V,F}
listarEncomendas( L ) :- solucoes((IdEncomenda,Nome, Peso, Volume,  PrecoEncomenda, Estado), 
                         encomenda(IdEncomenda,Nome, Peso, Volume,  PrecoEncomenda, Estado), R), 
                         diferentes(R, L).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Altera o estado de uma encomenda para finalizada e adiciona como concluida na base de conhecimento.
finalizaEncomenda(L) :- solucoes((L,Freguesia,Peso,Volume,Preco,_), 
                        encomenda(L,Freguesia,Peso,Volume,Preco,_), [R|T]), 
                        finalizaEncomendaAuxI(L), 
                        finalizaEncomendaAuxE(R),
                        eConcluida(L).

% Faz a involução da encomenda
finalizaEncomendaAuxI(L) :- involucao(encomenda(L,_,_,_,_,_)).

% Faz a evolucao da encomenda com o estado atualizado
finalizaEncomendaAuxE((L,Freguesia,Peso,Volume,Preco,_)) :- evolucao(encomenda(L, Freguesia, Peso, Volume, Preco, finalizado)).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Após uma encomenda ser dado como finalizada, aumenta a base do conhecimento com a conclusão de uma encomenda.
eConcluida(L) :- solucoes(R,entrega(R,_,L,_,_), [F|T]),
                 currentDate(D),
                 evolucao(concluido(F,L,D)).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Apresenta uma lista de todos as entregas da empresa
% Extensão do predicado listarEntregas: Lista -> {V,F}
listarEntregas( L ) :- solucoes((IdEntrega, Data, IdEncomenda, Prazo, Transporte), 
                       entrega(IdEntrega, Data, IdEncomenda, Prazo, Transporte), R), 
                       diferentes(R, L).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- QUERY 1 - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

estafetaEcologico(FINAL) :- listarEstafetas(R), estafetaEcologicoAux(R,100,_,FINAL).

estafetaEcologicoAux([],_,L,L).
estafetaEcologicoAux([R|T],MIN,L,FINAL) :- calcula(R, R1),
				     (R1 < MIN) -> estafetaEcologicoAux(T, R1, R,FINAL) ; estafetaEcologicoAux(T, MIN, L, FINAL).

calcula(R, L) :- solucoes(Id, estafeta(R, Id), R1), devolveListaVeiculos(R1, R2), calculaValorEcologico(R2, L).
					 
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- QUERY 2 - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Apresenta uma lista dos estafetas que entregaram uma encomenda a um determinado cliente
% Extensão do predicado estafetaCliente: Nome cliente, Lista -> {V,F}
estafetaCliente(NomeCliente, L) :- solucoes(ID, cliente(NomeCliente, ID, _), S), 
                                   diferentes(S, S1),
				   estafetaClienteAux(S1, L).

% Dado os Ids das Encomenda faz uma lista dos estafetas a que estavam associadas as encomendas
estafetaClienteAux([], []).							
estafetaClienteAux([ID|T] , L) :- solucoes(Nome, estafeta(Nome, ID), R1), 
                                  estafetaClienteAux(T, R2), 
                                  concatenar(R1, R2, L).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- QUERY 3 - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Apresenta uma lista dos clientes servidos por um determinado estafeta
% Extensão do predicado estafetaCliente: Nome cliente, Lista -> {V,F}
clienteEstafeta(NomeEstafeta, L) :- solucoes(ID, estafeta(NomeEstafeta, ID), S), 
                                    diferentes(S, S1),
				    clienteEstafetaAux(S1, L).
% Dados os Ids das encomendas faz uma lista dos clientes a que estavam associadas as encomendas
clienteEstafetaAux([],[]).
clienteEstafetaAux([ID|T], L) :- solucoes(Nome, cliente(Nome,ID, _), R1), 
                                 clienteEstafetaAux(T, R2), 
                                 concatenar(R1,R2,S), 
                                 diferentes(S,L).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- QUERY 4 - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Apresenta a faturação da empresa num determinado dia
% Extensão do predicado estafetaCliente: Data, Variável -> {V,F}
fatura(Data, L) :- solucoes(IdEncomenda, entrega(_, Data, IdEncomenda,_, _), S),
                   diferentes(S, S1),
		   faturaAux(S1, S2), somaC(S2,L).

% Dados os Ids das encomentas de um determinado dia calcula a faturação
faturaAux([],[]).
faturaAux([ID|T], L) :- solucoes(Preco, encomenda(ID, _, _, _, Preco,_), R1),
                        faturaAux(T,R2), 
                        concatenar(R1,R2, L).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- QUERY 5 - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Apresenta a freguesia com maior volume de encomendas
% Extensão do predicado freguesiaPlus: Variavel -> {V,F}
freguesiaPlus(Final) :- solucoes(Freguesia, encomenda(_,Freguesia,_,_,_,_), R), freguesiaPlusAux(R,0,_, Final).


freguesiaPlusAux([], _, L, L).     
freguesiaPlusAux([R|T],N, L, Final) :-  count(R, [R|T], N1),
                                         N1 > N -> removeAll(R, [R|T], S), freguesiaPlusAux(S, N1, R, Final) 
                                                 ; removeAll(R, [R|T], S), freguesiaPlusAux(S, N, L, Final).
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- QUERY 6 - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Apresenta a classificacao media de satisfação de um estafeta
% Extensão do predicado satisfacao: Nome Estafeta, Variável -> {V,F}
satisfacao(Nome, L) :- solucoes(ID, estafeta(Nome, ID), S), 
                       diferentes(S, S1),
		       satisfacaoAux(S1, L1), media(L1, L).

% Dado uma lista de Ids de encomendas calcula as classificações dadas pelos clientes.
satisfacaoAux([],[]).
satisfacaoAux([ID|T], L) :- solucoes(Classificacao, cliente(_, ID, Classificacao), S), 
                            satisfacaoAux(T, S1), 
                            concatenar(S,S1, L).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- QUERY 7 - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Apresenta o número total de entregas pelo diferentes meios de transporte num determinado intervalo de tempo
% Extensão do predicado totalEntregasVeiculo: DataI, DataF, Bicicleta/Moto/Carro -> {V,F}

totalEntregasVeiculo(D1,D2,B/M/C) :- totalEntregasIntervaloTempo(D1,D2,L),
        totalEntregasBic(L,B),
        totalEntregasMoto(L,M),
        totalEntregasCarro(L,C).

totalEntregasIntervaloTempo(D1,D2,L) :- listarEntregas( Entregas ),
        totalEntregasIntervaloTempoAux(D1,D2,Entregas,L).
        
totalEntregasIntervaloTempoAux(_,_,[],[]).
totalEntregasIntervaloTempoAux(D1,D2,[(IdEntrega, Data, IdEncomenda, Prazo, Transporte, Estado)|T],[(IdEntrega, Data, IdEncomenda, Prazo, Transporte, Estado)|T1]) :- 
        antesDe(Data,D2),depoisDe(Data,D1),!,totalEntregasIntervaloTempoAux(D1,D2,T,T1).
totalEntregasIntervaloTempoAux(D1,D2,[(IdEntrega, Data, IdEncomenda, Prazo, Transporte, Estado)|T],T1) :-
        totalEntregasIntervaloTempoAux(D1,D2,T,T1).

totalEntregasCarro([],0).
totalEntregasCarro([(_,_,_,_,carro)|T],X) :- totalEntregasCarro(T,Y),!, X is Y+1.
totalEntregasCarro([_|T],X) :- totalEntregasCarro(T,X).

totalEntregasMoto([],0).
totalEntregasMoto([(_,_,_,_,moto)|T],X) :- totalEntregasMoto(T,Y),!,X is Y+1.
totalEntregasMoto([_|T],X) :- totalEntregasMoto(T,X).

totalEntregasBic([],0).
totalEntregasBic([(_,_,_,_,bicicleta)|T],X) :- totalEntregasBic(T,Y),!, X is Y+1.
totalEntregasBic([_|T],X) :- totalEntregasBic(T,X).

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
diferentes( [X|L],[X|NL] ) :- removerElemento( L,X,TL ), 
                              diferentes( TL,NL ).

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
comprimento([_|T],R) :- comprimento(T,N), R is N+1.

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

contem(H, [H|_]).
contem(X, [_|T]) :- contem(X, T).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Faz a media de uma lista
% Extensao do predicado media: L,R -> {V,F}

media([H|T],S) :- somaC([H|T],S1), 
                  comprimento([H|T],S2), S is S1/S2.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Remove os elementos repetidos de uma lista
removeAll(_, [], []).
removeAll(X, [X|T], L):- removeAll(X, T, L), !.
removeAll(X, [H|T], [H|L]):- removeAll(X, T, L ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Conta o numero de elementos iguais numa lista
count(_, [], 0).
count(X, [X | T], N) :- !, count(X, T, N1), N is N1 + 1.
count(X, [_ | T], N) :- count(X, T, N).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Calcula o Valor Ecologigo de uma Lista de Transportes
calculaValorEcologico([T], L) :- valorEcologico(T, L).
calculaValorEcologico([H|T], L) :- valorEcologico(H,R), 
                                   calculaValorEcologico(T,RL), L is R+RL.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Dado uma Lista de Ids, devolve a lista de veiculos usados.
devolveListaVeiculos([], []).
devolveListaVeiculos([H|T], L) :- solucoes(Veiculo, entrega(H, _, _, _, Veiculo), R), 
                                  devolveListaVeiculos(T,R1), 
                                  concatenar(R,R1,L).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Devolve o valor ecologico de cada veiculo.
valorEcologico(bicicleta, L):- L is 1.
valorEcologico(moto, L):- L is 2.
valorEcologico(carro, L):- L is 3.
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Devolve a velocidade media de cada veiculo.
velocidade(bicicleta, L) :- L is 10.
velocidade(moto, L) :- L is 35.
velocidade(carro, L) :- L is 25.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Testa se um meio de transporte é válido.
testaTransporte(bicicleta).
testaTransporte(moto).
testaTransporte(carro).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Testa se uma data é valida.
testaData(date(Y,M,D)) :- Y > 2000, Y < 2025, M > 0, M < 13, D > 0, D < 32.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Devolve a data atual
currentDate(Today) :-
    get_time(Stamp),
    stamp_date_time(Stamp,DateTime,local),
    date_time_value(date,DateTime,Today).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Dado o id de uma encomenda devolve o Peso desta.
getPeso(Id,L) :- solucoes(Peso, encomenda(Id,_,Peso,_,_,_),[R|T]), L is R.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Dado um peso, vê se o meio de transporte é adequado
testaPesoTransporte(L, carro) :- L =< 100 .
testaPesoTransporte(L, moto) :- L =< 20 .
testaPesoTransporte(L, bicicleta) :- L =< 5 .

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% testa se uma classificação é válida.
testaClassificacao(L) :- L >= 0, L =< 5.

% Predicados sobre Datas
antesDe(D1/M1/A1,D2/M2/A2) :- A1 < A2.
antesDe(D1/M1/A1,D2/M2/A1) :- M1 < M2.
antesDe(D1/M1/A1,D2/M1/A1) :- D1 =< D2.

depoisDe(D1/M1/A1,D2/M2/A2) :- A1 > A2.
depoisDe(D1/M1/A1,D2/M2/A1) :- M1 > M2.
depoisDe(D1/M1/A1,D2/M1/A1) :- D1 >= D2.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%----------------------------- Invariantes - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- Encomenda - - - - -  -  -  -  -  -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariante Estrutural -> não permite a inserção caso já exista uma encomenda com esse IdEncomenda
+encomenda(IdEncomenda,_,_,_,_,_) :: (solucoes((IdEncomenda), (encomenda(IdEncomenda,_,_,_,_,_)), R), 
                                      comprimento(R, L), L == 1).

% Invariante Estrutural -> não permite a inserção caso seja dado um Peso negativo
+encomenda(_,_,Peso,_,_,_) :: naoNegativo(Peso).

% Invariante Estrutural -> não permite a inserção caso seja dado um Volume negativo
+encomenda(_,_,_,Volume,_,_) :: naoNegativo(Volume).

% Invariante Estrutural -> não permite a inserção caso seja inserido um preço negativo
+encomenda(_,_,_,_,Preco,_) :: naoNegativo(Preco).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- Entrega -  - - - - -  -  -  -  -  -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariante Estrutural -> não permite a inserção caso já exista uma entrega associado a esse IdEntrega
+entrega(IdEntrega,_,_,_,_) :: (solucoes((IdEntrega), (entrega(IdEntrega,_,_,_,_)), R),
                                comprimento(R, L), L == 1).

% Invariante Estrutural -> não permite a inserção caso a data seja inválida
+entrega(_,date(Y,M,D),_,_,_) :: testaData(date(Y,M,D)).      

% Invariante Estrutural -> não permite a inserção caso o prazo seja negativo
+entrega(_,_,_,Prazo,_) :: naoNegativo(Prazo).

% Invariante Estrutural -> não permite a inserção caso já existe uma entrega associada a uma encomenda
+entrega(_,_,IdEncomenda,_,_) :: (solucoes((IdEncomenda), (entrega(_,_,IdEncomenda,_,_)), S),
                                  comprimento(S,L), L == 1).

% Invariante Estrutural -> não permite a inserção caso não exista uma encomenda associada a esse IdEncomenda
+entrega(_,_,IdEncomenda,_,_) :: (solucoes((IdEncomenda), (encomenda(IdEncomenda,_,_,_,_,_)), S),
                                  comprimento(S,L), L == 1).         

% Invariante Estrutural -> não permite a inserção caso não seja inserido um meio de transporte válido
+entrega(_,_,_,_,Transporte) :: testaTransporte(Transporte).    

% Invariante Estrutural -> não permite a inserção caso o peso da encomenda associada seja superior ao do transporte
+entrega(_,_,IdEncomenda,_,Transporte) :: (getPeso(IdEncomenda,L), testaPesoTransporte(L, Transporte)). 

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- Estafeta - - - - - -  -  -  -  -  -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariante Estrutural -> não permite a inserção de um estafeta associado a uma entrega que não existe
+estafeta(_, IdEntrega) :: (solucoes((IdEntrega), (entrega(IdEntrega,_,_,_,_)), S),
                            comprimento(S,L), L == 1).

% Invariante Estrutural -> não permite a inserção de um estafeta associado a uma entrega que já estava associada a outro estafeta
+estafeta(_, IdEntrega) :: (solucoes((IdEntrega), (estafeta(_, IdEntrega)), S),
                            comprimento(S,L), L == 1).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- Cliente  - - - - - -  -  -  -  -  -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariante Estrutural -> não permite a inserção caso o cliente dê uma classificação não elegivel
+cliente(_,_,L) :- testaClassificacao(L).

% Invariante Estrutural -> não permite a inserção caso não seja inserido um meio de transporte válido
+cliente(_,IdEntrega,_) :- (solucoes((IdEntrega), (entrega(IdEntrega,_,_,_,_)), S),
                            comprimento(S,L), L == 1).

