

%----------------Dividir lista BFS

buscarPrimeirosXelementos(0,_,[]).
buscarPrimeirosXelementos(_,[],[]).
buscarPrimeirosXelementos(N,[H|T],[H|T1]) :- N>=0, N1 is N-1, buscarPrimeirosXelementos(N1,T,T1).

buscarUltimosXelementos(N,L,R):- reverse(L,L1), buscarPrimeirosXelementos(N, L1 ,LR), reverse(LR,R). 

calcDistIdBF(IdA,IdB,Distancia):- getFreguesia(IdA,FA), getFreguesia(IdB,FB), larguraprimeiroBF( FA,FB,_,Distancia). %distancia entre duas encomendas

seconds([],[]). %lista de pares -> lista com os segundos elementos de cada par
seconds([(_,B)|T], [B|Tail]):-seconds(T,Tail).
                        
ordenaPorDistanciaBF(IdHead, Tail , Ans) :- findall((Distancia,Id),(member(Id,Tail),calcDistIdBF(IdHead,Id,Distancia)),List),
                                                msort(List,Sorted),
                                                seconds(Sorted,Ans). %so testei com o 1,3,10,11
                                                
splitBF([H|T],L1,L2):-ordenaPorDistanciaBF(H,T,Lista), splitPorDistancia(H,Lista,L1,L2),!.



%----------------Dividir lista DFS

calcDistIdDF(IdA,IdB,Distancia):- getFreguesia(IdA,FA), getFreguesia(IdB,FB), 
                                  procuraProfundidadeVarios(FA,FB,Caminho),
                                  custo(Caminho,Distancia). %distancia entre duas encomendas

ordenaPorDistanciaDF(IdHead, Tail , Ans) :- findall((Distancia,Id),(member(Id,Tail),calcDistIdDF(IdHead,Id,Distancia)),List),
                                            msort(List,Sorted),
                                            seconds(Sorted,Ans). %so testei com o 1,3,10,11


splitDF([H|T],L1,L2):-ordenaPorDistanciaDF(H,T,Lista), splitPorDistancia(H,Lista,L1,L2),!.

%----------------Dividir lista AP
calcDistIdAP(IdA,IdB,Distancia):- getFreguesia(IdA,FA), getFreguesia(IdB,FB), 
                                  aprofundamentoProgress(FA,FB,_,Distancia).
                                 
ordenaPorDistanciaAP(IdHead, Tail , Ans) :- findall((Distancia,Id),(member(Id,Tail),calcDistIdAP(IdHead,Id,Distancia)),List),
                                            msort(List,Sorted),
                                            seconds(Sorted,Ans). %so testei com o 1,3,10,11


splitAP([H|T],L1,L2):-ordenaPorDistanciaAP(H,T,Lista), splitPorDistancia(H,Lista,L1,L2),!.


%---------------Split Genérica

splitPorDistancia(H,Lista,[H|T1],T2) :- %para esta função funcionar pra todos era fixe receber a lista já ordenada como argumento(Done)
                        length(Lista,Comp),
                        CompTotal is Comp + 1,
                        LenL is div(CompTotal,2) + mod(CompTotal,2) -1 , %-1 porque a head nao conta para ir buscar a lista
                        LenR is div(CompTotal,2) ,
                        buscarPrimeirosXelementos(LenL,Lista,T1),
                        buscarUltimosXelementos(LenR,Lista,T2).


