%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais

:- dynamic encomenda/6.
:- dynamic entrega/5.
:- dynamic estafeta/2.
:- dynamic cliente/3.
:- dynamic concluido/3.
:- discontiguous estimaB/3.
:- discontiguous estima/3.

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
concluido(6, 5, date(2021,6,9)).
concluido(3, 6, date(2021,11,12)).
concluido(9, 9, date(2021,6,9)).
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Encomenda
% Extensao do predicado encomenda : IdEncomenda, Freguesia, Peso, Volume, Preço, Estado -> { V, F }
encomenda(1, sameiro,     5,   5,  50,   pendente).
encomenda(2, maximinos, 3,  10, 60,   caminho).
encomenda(3, bomJesus,     12,  67, 250,  pendente).             
encomenda(4, lamacaes,  10,  18, 27,   finalizada).
encomenda(5, gualtar,   2,   2,  1500, finalizada).
encomenda(6, lomar,     70, 4,  30,   caminho).
encomenda(7, bomJesus,     30,   4,  42, finalizada).
encomenda(8, merelim,   20,   3,  25, caminho).
encomenda(9, saoVitor, 20, 2, 21, finalizada).
encomenda(10,saoVicente, 8, 3, 20, pendente).   
encomenda(11, vilaVerde, 3, 7, 25, pendente).
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Entrega       
% Extensao do predicado entrega : IdEntrega, Data, IdEncomenda, Prazo, Transporte -> { V, F }
entrega( 4, date(2021,10,2), 2, 13, bicicleta).
entrega( 2, date(2021,10,3), 3, 1, moto).
entrega( 1, date(2021,11,4), 1, 3, moto).
entrega( 3, date(2021,11,7), 6, 3, carro). 
entrega( 5, date(2021,11,7), 4, 2, moto).
entrega( 6, date(2021, 6,6), 5, 7, bicicleta).
entrega( 7, date(2021, 6,4), 7, 8, carro).
entrega( 8, date(2021, 9,10), 8, 5, carro).
entrega( 9, date(2021, 6,1), 9, 20, carro).
entrega( 10, date(2022, 1, 3), 10, 10, bicicleta).
entrega( 11, date(2021, 12, 25), 11, 15, bicicleta).
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

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
estafeta(andre,9).
estafeta(andre, 10).
estafeta(maria, 11).
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
cliente(ze, 9, 2).
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
inicial(centroDeRecolha).
final(centroDeRecolha).

aresta(sameiro, lamacaes, 0.7).
aresta(sameiro, bomJesus, 1.3).
aresta(bomJesus, lamacaes, 2).
aresta(bomJesus, centroDeRecolha, 5.6).
aresta(lamacaes, lomar, 5.4).
aresta(lamacaes, universidade, 3.1).
aresta(lomar, saoVitor, 7.3).
aresta(lomar, maximinos, 3.1).
aresta(centroDeRecolha, maximinos, 6.9).
aresta(saoVitor, maximinos, 3.6).
aresta(centroDeRecolha, universidade, 6.1).
aresta(centroDeRecolha, lamacaes, 5.1).
aresta(universidade, saoVicente, 4.3).
aresta(maximinos, saoVicente, 8).
aresta(saoVicente, merelim, 4.7).
aresta(saoVicente, amares, 11.8).
aresta(saoVicente, vilaVerde, 12.6).

%estima(Destino,Km,Minutos)
estimaC(sameiro,8.6,12).
estimaC(bomJesus,5.6,8).
estimaC(lamacaes,5.1,8).
estimaC(lomar,8.3,12).
estimaC(universidade,6.1,10).
estimaC(saoVitor,3.8,5).
estimaC(maximinos,6.9,9).
estimaC(saoVicente,6.4,9).
estimaC(merelim,10.9,14).
estimaC(amares,11,14).
estimaC(vilaVerde,17.1,20).
estimaC(centroDeRecolha,0,0).

estimaM(Local,Km,NewTmp) :- estimaC(Local,Km,Tmp), NewTmp is (0.9 * Tmp). 

estimaB(sameiro,6.4,49).
estimaB(bomJesus,3.9,28).
estimaB(lamacaes,4.7,16).
estimaB(lomar,7.9,24).
estimaB(universidade,2.3,7).
estimaB(saoVitor,3.4,9).
estimaB(maximinos,7.6,24).
estimaB(saoVicente,5.9,18).
estimaB(merelim,10.7,31).
estimaB(amares,10.5,39).
estimaB(vilaVerde,11.8,43).
estimaB(centroDeRecolha,0,0).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%-----------------------------	PREDICADOS  - - - - -  - - - - -  -  -  
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
connected(X,Y) :- aresta(X,Y,_).

membro(X, [X|_]).
membro(X, [_|Xs]):- membro(X, Xs).

move(X,Y,Custo) :- aresta(X,Y,Custo).
move(X,Y,Custo) :- aresta(Y,X,Custo).

%------------Profundidade DFS-----------------------------
/*testa(Caminhos) :- multi(Caminhos), printCircuitos(Caminhos).

printCircuitos([]).
printCircuitos([L|T]) :- writeln(L), printCircuitos(T).


multi(Caminhos):-   findall(Freguesia, encomenda(_, Freguesia,_,_,_,pendente), PorPassar),
                    findall(Caminho, 
                    (algoritmoDFSAux(_,Caminho),containsAll(PorPassar, Caminho))
                    ,Caminhos).
                    
containsAll([],_).
containsAll([H|T], L1):- member(H,L1),containsAll(T,L1).
*/

getPrazo(IdEncomenda, Prazo) :- entrega(IdEntrega,_,IdEncomenda, Prazo,_).
getFreguesia(IdEncomenda, Freguesia) :- encomenda(IdEncomenda, Freguesia,_,_,_,_).

ordenaPrazo([], []).
ordenaPrazo([H|T], R):- ordenaPrazo(T, R1), insert_ordPrazo(H, R1, R).

insert_ordPrazo(X, [H|T], [H|R]):- getPrazo(X,A) , getPrazo(H,B), A >B, !, insert_ordPrazo(X, T, R).
insert_ordPrazo(X, T, [X|T]).


getFreguesias([Id], [Freguesia]) :- encomenda(Id,Freguesia,_,_,_,_).
getFreguesias([Id|T], [Freguesia|Lista]) :- encomenda(Id,Freguesia,_,_,_,_), getFreguesias(T, Lista).





listadeRotas(IdsEncomendas, Caminho) :- ordenaPrazo(IdsEncomendas, IdsOrdenados), 
                                       getFreguesias(IdsOrdenados, Freguesias),
                                       append([centroDeRecolha],Freguesias, NewFreguesias),
                                       append(NewFreguesias, [centroDeRecolha], FreguesiasC),
                                       calculaRota(FreguesiasC, Caminho).

calculaRota([T], Caminho).
calculaRota([X,Y], [Caminho]) :- procuraProfundidadeVarios(X,Y, Caminho).
calculaRota([X,Y|T], [Z|Caminho]) :- procuraProfundidadeVarios(X,Y,Z), calculaRota([Y|T],Caminho).


procuraProfundidadeVarios(Origem, Destino, Caminho) :- dfs(Origem,Destino,Caminho).

dfs(Orig,Dest,Cam) :-  dfs2(Orig,Dest,[Orig],Cam).
dfs2(Dest,Dest,LA,Cam) :- reverse(LA,Cam).
dfs2(Act,Dest,LA,Cam) :-  move(Act,X,_),
                          \+ member(X,LA),
                          dfs2(X,Dest,[X|LA],Cam). 

%---------------------------------------------------------
%------------Profundidade DFS-----------------------------
%---------------------------------------------------------

algoritmoDFS(X) :- solucoes((IdEncomenda, Circuito), (encomenda(IdEncomenda,_,_,_,_,pendente), algoritmoDFSAux(IdEncomenda, Circuito)), X).


algoritmoDFSAux(IdEncomenda,Caminho) :- encomenda(IdEncomenda, Freguesia, Peso, _, _, pendente),
                             entrega(_, _, IdEncomenda, Prazo, _),
                             procuraProfundidade(Freguesia, Caminho, Custo),
                             escolheTransporte(Peso, Custo, Prazo, Transporte),
                             calculaTempo(Transporte, Peso, Custo, Tempo),
                             printCircuito(Caminho,Custo,Tempo,Transporte),!.

printCircuito(Caminho,Distancia,TempoEntrega,Transporte) :-
	write('Caminho = '),writeln(Caminho),
	write('Distancia = '),write(Distancia),writeln(' km'),
	write('Tempo de entrega = '),write(TempoEntrega),writeln(' horas'),
	write('Tipo de transporte = '),writeln(Transporte),writeln('').

procuraProfundidade(Destino, Caminho, Custo) :- resolve_pp_c(Destino,C,Custo),reverse(C, Caminho).

resolve_pp_c(Nodo, [Nodo|Caminho], C) :-
    profundidadeprimeiro(Nodo,[Nodo], Caminho , C).

profundidadeprimeiro(Nodo,_,[],0) :- final(Nodo).
profundidadeprimeiro(Nodo, Historico, [ProxNodo|Caminho], C):-
    move(Nodo,ProxNodo, C1),
    not(member(ProxNodo,Historico)),
    profundidadeprimeiro(ProxNodo,[ProxNodo|Historico],Caminho, C2), C is C1+C2.

%------------Auxiliares-----------------------------

escolheTransporte(Peso, Custo, Prazo, bicicleta) :- Peso =< 5, calculaTempo(bicicleta, Peso, Custo, Tempo) , Tempo =< Prazo.
escolheTransporte(Peso, Custo, Prazo, moto) :- Peso =< 20, calculaTempo(moto, Peso, Custo, Tempo) , Tempo =< Prazo.
escolheTransporte(Peso, Custo, Prazo, carro) :- Peso =< 100, calculaTempo(carro, Peso, Custo, Tempo) , Tempo =< Prazo.
        

calculaTempo(carro,Peso,Distancia,Tempo) :-
    VelocidadeMedia is (25 - (0.1 * Peso)),
    Tempo is (Distancia / VelocidadeMedia).

calculaTempo(bicicleta,Peso,Distancia,Tempo) :-
    VelocidadeMedia is (10 - (0.7 * Peso)),
    Tempo is (Distancia / VelocidadeMedia).

calculaTempo(moto,Peso,Distancia,Tempo) :-
    VelocidadeMedia is (35 - (0.5 * Peso)),
    Tempo is (Distancia / VelocidadeMedia).

%------------Largura BFS-----------------------------
%Largura (BFS - Breadth-First Search)


algoritmoBFS(X) :- solucoes((IdEncomenda, Circuito), (encomenda(IdEncomenda,_,_,_,_,pendente), algoritmoBFSAux(IdEncomenda, Circuito)), X).

algoritmoBFSAux(IdEncomenda,Caminho) :- encomenda(IdEncomenda, Freguesia, Peso, _, _, pendente),
                                        entrega(_, _, IdEncomenda, Prazo, _),
                                        larguraprimeiroBF(Freguesia, Caminho, Custo),
                                        escolheTransporte(Peso, Custo, Prazo, Transporte),
                                        calculaTempo(Transporte, Peso, Custo, Tempo),
                                        printCircuito(Caminho,Custo,Tempo,Transporte),!.

larguraprimeiroBF(Dest, Cam,Custo):- larguraprimeiro(Dest,[[centroDeRecolha]],Cam),
        custo(Cam,Custo).

larguraprimeiro(Dest, [[Dest|Tail]|_], Caminho) :- reverse([Dest|Tail],Caminho).
larguraprimeiro(Dest, [Largura|Outros], Caminho) :- 
    Largura=[NodoAtual|_],
    findall([X|Largura],
    (Dest\==NodoAtual, move(NodoAtual,X,_), not(member(X,Largura))), Novos),
    append(Outros, Novos, Todos),
    larguraprimeiro(Dest, Todos, Caminho).


custo([A,B],Custo) :- move(A,B,Custo).
custo([A,B|T],Custo) :- move(A,B,C),
        custo([B|T],NewCusto),
        Custo is NewCusto+C.

%------------Algoritmo Pesquisa Iterativa Aprofundamento Progressivo----------------------------
algoritmoAP(X) :- solucoes((IdEncomenda, Circuito), (encomenda(IdEncomenda,_,_,_,_,pendente), algoritmoAPAux(IdEncomenda, Circuito)), X).


algoritmoAPAux(IdEncomenda,Caminho) :-  encomenda(IdEncomenda, Freguesia, Peso, _, _, pendente),
                                        entrega(_, _, IdEncomenda, Prazo, _),
                                        aprofundamentoProgress(Freguesia, Caminho, Custo),
                                        escolheTransporte(Peso, Custo, Prazo, Transporte),
                                        calculaTempo(Transporte, Peso, Custo, Tempo),
                                        printCircuito(Caminho,Custo,Tempo,Transporte),!.


limite(50). %custo máximo do caminho em profundidade 

aprofundamentoProgress(Destino,Caminho,Custo):- limitadaAux(Destino,Caminho,0,Custo). 

limitadaAux(_,_,Y,_):- limite(K), K=<Y, fail.
limitadaAux(Destino,Caminho,Iter,Custo):- limitadaProfundidade(Destino,Caminho,Iter,Custo),!.
limitadaAux(Destino,Caminho,Iter,Custo):- limite(Y), Iter<Y ,X is Iter+1 ,limitadaAux(Destino,Caminho,X,Custo).

limitadaProfundidade(Destino,Caminho,Limite,Custo):-
        procuraProfundidade(Destino,Caminho,Custo), Custo > 0 , Custo=<Limite.



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- QUERY 1 - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Apresenta o estafeta que utilizou mais vezes um meio de transporte ecológico.
% Extensão do predicado estafetaEcologico: Nome cliente, Lista -> {V,F}
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
                                  concatenar(R1, R2, R3),
                                  diferentes(R3,L).

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
		   faturaAux(S,L).

% Dados os Ids das encomentas de um determinado dia calcula a faturação
faturaAux([],0).
faturaAux([ID|T], L) :- encomenda(ID, _, _, _, Preco,_),
                        faturaAux(T,OldL),L is Preco + OldL.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- QUERY 5 - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Apresenta a freguesia com maior volume de encomendas
% Extensão do predicado freguesiaPlus: Variavel -> {V,F}
freguesiaPlus(Final) :- solucoes(Freguesia, encomenda(_,Freguesia,_,_,_,_), R),diferentes(R,R1), freguesiaPlusAux(R1,0,_, Final).


freguesiaPlusAux([], _, L, L).     
freguesiaPlusAux([R|T],N, L, Final) :-  count(R, [R|T], N1),
                                         N1 > N -> removeAll(R, [R|T], S), freguesiaPlusAux(S, N1, R, Final) 
                                                 ; removeAll(R, [R|T], S), freguesiaPlusAux(S, N, L, Final).
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- QUERY 6 - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Apresenta a classificacao media de satisfação de um estafeta
% Extensão do predicado satisfacao: Nome Estafeta, Variável -> {V,F}
satisfacao(Nome, L) :-  solucoes(ID, estafeta(Nome, ID), S), 
		        satisfacaoAux(S, L1),length(S, X),
                        L is L1/X.

% Dado uma lista de Ids de encomendas calcula as classificações dadas pelos clientes.
satisfacaoAux([],0).
satisfacaoAux([ID|T], Res) :- cliente(_, ID, Classificacao),!, 
                              satisfacaoAux(T, Y) , Res is Classificacao+Y.
                        

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
totalEntregasIntervaloTempoAux(D1,D2,[(IdEntrega, D3, IdEncomenda, Prazo, Transporte)|T],[(IdEntrega, D3, IdEncomenda, Prazo, Transporte)|T1]) :- 
        antesDe(D3,D2),
        depoisDe(D3,D1),
        !,
        totalEntregasIntervaloTempoAux(D1,D2,T,T1).
totalEntregasIntervaloTempoAux(D1,D2,[_|T],T1) :-
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

totalEntregasEstafeta(D1,D2,Res) :- totalEntregasIntervaloTempo(D1,D2,L), %(Entregas)
        totalEntregasEstafetaAux(L,NewL),
        agroup(NewL,Res).

totalEntregasEstafetaAux([],[]).
totalEntregasEstafetaAux([(IdEntrega, _, _, _, _)|T],[(Nome,1)|T1]) :- estafeta(Nome,IdEntrega),
        !,
        totalEntregasEstafetaAux(T,T1).

agroup([],[]).
agroup([(A,X)|Tail],[(A,Z)|Res]):-member((A,_),Tail), 
                conta(A,Tail,Y), 
                removeParesFirst(A,Tail,Tmp),
                agroup(Tmp,Res), Z is Y+X.
agroup([(A,X)|Tail],[(A,X)|Res]):-not(member((A,_),Tail)),agroup(Tail,Res).

removeParesFirst(_,[],[]).
removeParesFirst(A,[(A,_)|Tail], Res):-removeParesFirst(A,Tail,Res).
removeParesFirst(A,[(B,K)|Tail], [(B,K)|Res]):- B\=A, removeParesFirst(A,Tail,Res).

conta(_,[],0).
conta(A,[(B,_)|Tail],Y):- B\=A, conta(A,Tail,Y).
conta(A,[(A,X)|Tail],Y):-conta(A,Tail,X2), Y is X2+X.
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- QUERY 9 - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

totalEntreguesENaoEntregues(D1,D2,Ent/NEnt) :- totalEntregasIntervaloTempo(D1,D2,L), totalEntreguesENaoEntreguesAux(D1,D2,L,Ent/NEnt).

totalEntreguesENaoEntreguesAux(_,_,[],0/0).
totalEntreguesENaoEntreguesAux(D1,D2,[(IdEntrega, _, _, _, _)|T],Ent/NEnt) :- 
        \+concluido(IdEntrega,_,_),!, totalEntreguesENaoEntreguesAux(D1,D2,T,Ent/OldNEnt), NEnt is OldNEnt + 1.
totalEntreguesENaoEntreguesAux(D1,D2,[(IdEntrega, _, _, _, _)|T],Ent/NEnt) :-
        concluido(IdEntrega,_,D3), antesDe(D3,D2), depoisDe(D3,D1),!, totalEntreguesENaoEntreguesAux(D1,D2,T, OldEnt/NEnt), Ent is OldEnt + 1.
totalEntreguesENaoEntreguesAux(D1,D2,[_|T],Ent/NEnt) :- totalEntreguesENaoEntreguesAux(D1,D2,T,Ent/OldNEnt), NEnt is OldNEnt + 1.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- QUERY 10 - - - - - -  -  -  -  -  -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Devolve uma lista com os estafetas e o peso que cada um leva numa data em que se entregaram encomendas.
% Extensao do predicado peso transportado: Data, Variavel -> {V,F}
pesoTransportado(X, L) :- listarEstafetas(T), procura(T, X, L). 

% Dada uma lista de estafetas, calcula o peso que cada um carregava recursivamente
procura([],_,[]).
procura([R|T], X, [R/F|L]) :-  calculaIdsEntrega(R, X, F),!, procura(T, X, L).

% Dado o nome de um estafeta, calcula todos os IdEntrega associados a ele
calculaIdsEntrega(R, X, L) :- solucoes(IdEntrega, estafeta(R,IdEntrega), T), calculaIdsEncomenda(T, X, L).

% Dado uma lista de IdsEntrega, verifica a data -> devolve o peso, faz a recursiva
calculaIdsEncomenda([], _, 0).
calculaIdsEncomenda([R|T], X, L) :-  \+ (concluido(R, _, X)) , calculaIdsEncomenda(T, X, L).
calculaIdsEncomenda([R|T], X, L) :- concluido(R, IdEncomenda, X), calculaPeso(IdEncomenda,F), 
                                    calculaIdsEncomenda(T, X, R1), L is R1 + F.

% Dado um IdEncomenda devolve o Peso associado
calculaPeso(R, Peso) :- encomenda(R,_,Peso,_,_,_). 
calculaPeso(_,0).
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%---------------------- Predicados Auxiliares  - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -

% Apresenta uma lista de todos os estafetas da empresa
% Extensão do predicado listarEstafetas: Lista -> {V,F}
listarEstafetas( L ) :- solucoes(Nome, estafeta(Nome, _), R), diferentes(R, L).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Apresenta uma lista de todos os clientes da empresa
% Extensão do predicado listarCliente: Lista -> {V,F}
listarCliente( L ) :- solucoes(Nome, cliente(Nome, _, _), R), 
                      diferentes(R, L).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Apresenta uma lista de todos as encomendas da empresa
% Extensão do predicado listarEncomendas: Lista -> {V,F}
listarEncomendas( L ) :- solucoes((IdEncomenda,Nome, Peso, Volume,  PrecoEncomenda, Estado), 
                         encomenda(IdEncomenda,Nome, Peso, Volume,  PrecoEncomenda, Estado), R), 
                         diferentes(R, L).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Altera o estado de uma encomenda para finalizada e adiciona como concluida na base de conhecimento.
finalizaEncomenda(L) :- solucoes((L,Freguesia,Peso,Volume,Preco,_), 
                        encomenda(L,Freguesia,Peso,Volume,Preco,_), [R|_]), 
                        finalizaEncomendaAuxI(L), 
                        finalizaEncomendaAuxE(R),
                        eConcluida(L).

% Faz a involução da encomenda
finalizaEncomendaAuxI(L) :- involucao(encomenda(L,_,_,_,_,_)).

% Faz a evolucao da encomenda com o estado atualizado
finalizaEncomendaAuxE((L,Freguesia,Peso,Volume,Preco,_)) :- evolucao(encomenda(L, Freguesia, Peso, Volume, Preco, finalizado)).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Após uma encomenda ser dado como finalizada, aumenta a base do conhecimento com a conclusão de uma encomenda.
eConcluida(L) :- solucoes(R,entrega(R,_,L,_,_), [F|_]),
                 currentDate(D),
                 evolucao(concluido(F,L,D)).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Apresenta uma lista de todos as entregas da empresa
% Extensão do predicado listarEntregas: Lista -> {V,F}
listarEntregas( L ) :- solucoes((IdEntrega, Data, IdEncomenda, Prazo, Transporte), 
                       entrega(IdEntrega, Data, IdEncomenda, Prazo, Transporte), R), 
                       diferentes(R, L).

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

somaC([],0).
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

media(L,S) :- somaC(L,S1), 
                  comprimento(L,S2), S is S1/S2.

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
getPeso(Id,Peso) :- encomenda(Id,_,Peso,_,_,_).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Dado um peso, vê se o meio de transporte é adequado
testaPesoTransporte(L, carro) :- L =< 100 .
testaPesoTransporte(L, moto) :- L =< 20 .
testaPesoTransporte(L, bicicleta) :- L =< 5 .

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% testa se uma classificação é válida.
testaClassificacao(L) :- L >= 0, L =< 5.

% Predicados sobre Datas
antesDe(date(A1,_,_),date(A2,_,_)) :- A1 < A2,!.
antesDe(date(A1,M1,_),date(A1,M2,_)) :- M1 < M2,!.
antesDe(date(A1,M1,D1),date(A1,M1,D2)) :- D1 =< D2.

depoisDe(date(A1,_,_),date(A2,_,_)) :- A1 > A2,!.
depoisDe(date(A1,M1,_),date(A1,M2,_)) :- M1 > M2,!.
depoisDe(date(A1,M1,D1),date(A1,M1,D2)) :- D1 >= D2.

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
+cliente(_,_,L) :: testaClassificacao(L).

% Invariante Estrutural -> não permite a inserção caso não seja inserido um meio de transporte válido
+cliente(_,IdEntrega,_) :: (solucoes((IdEntrega), (entrega(IdEntrega,_,_,_,_)), S),
                            comprimento(S,L), L == 1).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%--------------------------------- Concluida  - - - - - -  -  -  -  -  -
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariante Estrutural -> não permite a inserção caso a data de finalização não seja válida
+concluido(_,_,L) :: testaData(L).
% Invariante Estrutural -> não permite a inserção caso a encomenda já não esteja inserida na base
% de conhecimento como finalizada
+concluido(_,L,_) :: (solucoes((L), (encomenda(L,_,_,_,_,finalizada)), S),
                     comprimento(S,N), N == 1).
% Invariante Estrutural -> não permite a inserção caso a entrega associada não esteja inserida na
% base de conhecimento
+concluido(L,_,_) :: (solucoes((L), (entrega(L,_,_,_,_)), S),
                     comprimento(S,N), N == 1).
