circuitoAestrela(Nodo,Prazo,Peso,Caminho,bicicleta) :-
    resolve_aestrela(Nodo,bicicleta,Peso,Caminho),
    custo(Caminho,Distancia),
    calculaTempo(bicicleta,Peso,Distancia,Tempo),
    Tempo =< Prazo.
circuitoAestrela(Nodo,Prazo,Peso,Caminho,moto) :-
    resolve_aestrela(Nodo,moto,Peso,Caminho),
    custo(Caminho,Distancia),
    calculaTempo(moto,Peso,Distancia,Tempo),
    Tempo =< Prazo.
circuitoAestrela(Nodo,Prazo,Peso,Caminho,carro) :-
    resolve_aestrela(Nodo,carro,Peso,Caminho),
    custo(Caminho,Distancia),
    calculaTempo(carro,Peso,Distancia,Tempo),
    Tempo =< Prazo.


algoritmoAStar(X) :- solucoes((IdEncomenda, Circuito), (encomenda(IdEncomenda,_,_,_,_,pendente), algoritmoAStarAux(IdEncomenda, Circuito)), X).


algoritmoAStarAux(IdEncomenda,Caminho) :-  encomenda(IdEncomenda, Freguesia, Peso, _, _, pendente),
                                        entrega(_, _, IdEncomenda, Prazo, _),
                                        circuitoAestrela(Freguesia,Prazo, Peso,Caminho,Transporte),
                                        custo(Caminho,Custo),
                                        calculaTempo(Transporte, Peso, Custo, Tempo),
                                        printCircuito(Caminho,Custo,Tempo,Transporte),!.
%------------------------------------------------------------------------------------------------
%------------------------------------------------------------------------------------------------

resolve_aestrela(Nodo,carro,Peso,Caminho) :-
    Peso =< 100,
	estimaC(Nodo, EstimaD, EstimaT),
	aestrelaCarro([[Nodo]/0/0/EstimaD/EstimaT], InvCaminho/CustoKm/CustoTmp/_/_,Peso),
	reverse(InvCaminho, Caminho).

resolve_aestrela(Nodo,moto,Peso,Caminho) :-
    Peso =< 20,
	estimaM(Nodo, EstimaD, EstimaT),
	aestrelaMoto([[Nodo]/0/0/EstimaD/EstimaT], InvCaminho/CustoKm/CustoTmp/_/_,Peso),
	reverse(InvCaminho, Caminho).

resolve_aestrela(Nodo,bicicleta,Peso,Caminho) :-
    Peso =< 5,
	estimaB(Nodo, EstimaD, EstimaT),
	aestrelaBic([[Nodo]/0/0/EstimaD/EstimaT], InvCaminho/CustoKm/CustoTmp/_/_,Peso),
	reverse(InvCaminho, Caminho).

%------------------------------------------------------------------------------------------------
%---------------------------------------------  CARRO  ------------------------------------------

aestrelaCarro(Caminhos, Caminho,_) :-
	obtem_melhor_aeCarro(Caminhos, Caminho),
	Caminho = [Nodo|_]/_/_/_/_,
    final(Nodo).
aestrelaCarro(Caminhos, SolucaoCaminho,Peso) :-
	obtem_melhor_aeCarro(Caminhos, MelhorCaminho),
	seleciona(MelhorCaminho, Caminhos, OutrosCaminhos),
	expande_aestrelaCarro(MelhorCaminho, ExpCaminhos,Peso),
	append(OutrosCaminhos, ExpCaminhos, NovoCaminhos),
    aestrelaCarro(NovoCaminhos, SolucaoCaminho,Peso).	

obtem_melhor_aeCarro([Caminho], Caminho) :- !.
obtem_melhor_aeCarro([Caminho1/CustoKm1/CustoTmp1/EstKm1/EstTmp1,_/CustoKm2/CustoTmp2/EstKm2/EstTmp2|Caminhos], MelhorCaminho) :-
	CustoKm1 * 0.3 + CustoTmp1 * 0.7 + EstKm1 * 0.3 + EstTmp1 * 0.7 =< CustoKm2 * 0.3 + CustoTmp2 * 0.7 + EstKm2 * 0.3 + EstTmp2 * 0.7, !,
	obtem_melhor_aeCarro([Caminho1/CustoKm1/CustoTmp1/EstKm1/EstTmp1|Caminhos], MelhorCaminho). 
obtem_melhor_aeCarro([_|Caminhos], MelhorCaminho) :- 
	obtem_melhor_aeCarro(Caminhos, MelhorCaminho).
	
expande_aestrelaCarro(Caminho, ExpCaminhos, Peso) :-
	findall(NovoCaminho, adjacente_aeCarro(Caminho,NovoCaminho,Peso), ExpCaminhos).

adjacente_aeCarro([Nodo|Caminho]/CustoKm/CustoTmp/_/_,[ProxNodo,Nodo|Caminho]/NovoCustoKm/NovoCustoTmp/EstKm/EstTmp,Peso) :-
    move(Nodo,ProxNodo,PassoCustoKm),
    \+member(ProxNodo,Caminho),
    calculaTempo(carro,Peso,PassoCustoKm,PassoCustoTmp),
    NovoCustoKm is PassoCustoKm + CustoKm,
    NovoCustoTmp is PassoCustoTmp + CustoTmp,
    estimaC(ProxNodo,EstKm,EstTmp).

%------------------------------------------------------------------------------------------------
%---------------------------------------------  MOTO  ------------------------------------------

aestrelaMoto(Caminhos, Caminho, _) :-
	obtem_melhor_aeMoto(Caminhos, Caminho),
	Caminho = [Nodo|_]/_/_/_/_,
    final(Nodo).
aestrelaMoto(Caminhos, SolucaoCaminho,Peso) :-
	obtem_melhor_aeMoto(Caminhos, MelhorCaminho),
	seleciona(MelhorCaminho, Caminhos, OutrosCaminhos),
	expande_aestrelaMoto(MelhorCaminho, ExpCaminhos,Peso),
	append(OutrosCaminhos, ExpCaminhos, NovoCaminhos),
    aestrelaMoto(NovoCaminhos, SolucaoCaminho,Peso).	

obtem_melhor_aeMoto([Caminho], Caminho) :- !.
obtem_melhor_aeMoto([Caminho1/CustoKm1/CustoTmp1/EstKm1/EstTmp1,_/CustoKm2/CustoTmp2/EstKm2/EstTmp2|Caminhos], MelhorCaminho) :-
	CustoKm1 * 0.3 + CustoTmp1 * 0.7 + EstKm1 * 0.3 + EstTmp1 * 0.7 =< CustoKm2 * 0.3 + CustoTmp2 * 0.7 + EstKm2 * 0.3 + EstTmp2 * 0.7, !,
	obtem_melhor_aeMoto([Caminho1/CustoKm1/CustoTmp1/EstKm1/EstTmp1|Caminhos], MelhorCaminho). 
obtem_melhor_aeMoto([_|Caminhos], MelhorCaminho) :- 
	obtem_melhor_aeMoto(Caminhos, MelhorCaminho).
	
expande_aestrelaMoto(Caminho, ExpCaminhos, Peso) :-
	findall(NovoCaminho, adjacente_aeMoto(Caminho,NovoCaminho,Peso), ExpCaminhos).

adjacente_aeMoto([Nodo|Caminho]/CustoKm/CustoTmp/_/_,[ProxNodo,Nodo|Caminho]/NovoCustoKm/NovoCustoTmp/EstKm/EstTmp,Peso) :-
    move(Nodo,ProxNodo,PassoCustoKm),
    \+member(ProxNodo,Caminho),
    calculaTempo(moto,Peso,PassoCustoKm,PassoCustoTmp),
    NovoCustoKm is PassoCustoKm + CustoKm,
    NovoCustoTmp is PassoCustoTmp + CustoTmp,
    estimaM(ProxNodo,EstKm,EstTmp).

%------------------------------------------------------------------------------------------------
%-------------------------------------------- BICICLETA -----------------------------------------

aestrelaBic(Caminhos, Caminho, _) :-
	obtem_melhor_aeBic(Caminhos, Caminho),
	Caminho = [Nodo|_]/_/_/_/_,
    final(Nodo).
aestrelaBic(Caminhos, SolucaoCaminho,Peso) :-
	obtem_melhor_aeBic(Caminhos, MelhorCaminho),
	seleciona(MelhorCaminho, Caminhos, OutrosCaminhos),
	expande_aestrelaBic(MelhorCaminho, ExpCaminhos,Peso),
	append(OutrosCaminhos, ExpCaminhos, NovoCaminhos),
    aestrelaBic(NovoCaminhos, SolucaoCaminho,Peso).	

obtem_melhor_aeBic([Caminho], Caminho) :- !.
obtem_melhor_aeBic([Caminho1/CustoKm1/CustoTmp1/EstKm1/EstTmp1,_/CustoKm2/CustoTmp2/EstKm2/EstTmp2|Caminhos], MelhorCaminho) :-
	CustoKm1 * 0.8 + CustoTmp1 * 0.2 + EstKm1 * 0.8 + EstTmp1 * 0.2 =< CustoKm2 * 0.8 + CustoTmp2 * 0.2 + EstKm2 * 0.8 + EstTmp2 * 0.2,
    !,
	obtem_melhor_aeBic([Caminho1/CustoKm1/CustoTmp1/EstKm1/EstTmp1|Caminhos], MelhorCaminho). 
obtem_melhor_aeBic([_|Caminhos], MelhorCaminho) :- 
	obtem_melhor_aeBic(Caminhos, MelhorCaminho).
	
expande_aestrelaBic(Caminho, ExpCaminhos, Peso) :-
	findall(NovoCaminho, adjacente_aeBic(Caminho,NovoCaminho,Peso), ExpCaminhos).

adjacente_aeBic([Nodo|Caminho]/CustoKm/CustoTmp/_/_,[ProxNodo,Nodo|Caminho]/NovoCustoKm/NovoCustoTmp/EstKm/EstTmp,Peso) :-
    move(Nodo,ProxNodo,PassoCustoKm),
    \+member(ProxNodo,Caminho),
    calculaTempo(bicicleta,Peso,PassoCustoKm,PassoCustoTmp),
    NovoCustoKm is PassoCustoKm + CustoKm,
    NovoCustoTmp is PassoCustoTmp + CustoTmp,
    estimaB(ProxNodo,EstKm,EstTmp).
