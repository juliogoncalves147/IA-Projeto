/*
circuitoGulosa(Nodo,Prazo,Peso,Caminho,bicicleta) :-
    resolve_gulosa(Nodo,bicicleta,Peso,Caminho),
    custo(Caminho,Distancia),
    calculaTempo(bicicleta,Peso,Distancia,Tempo),
    Tempo =< Prazo.
circuitoGulosa(Nodo,Prazo,Peso,Caminho,moto) :-
    resolve_gulosa(Nodo,moto,Peso,Caminho),
    custo(Caminho,Distancia),
    calculaTempo(moto,Peso,Distancia,Tempo),
    Tempo =< Prazo.
circuitoGulosa(Nodo,Prazo,Peso,Caminho,carro) :-
    resolve_gulosa(Nodo,carro,Peso,Caminho),
    custo(Caminho,Distancia),
    calculaTempo(carro,Peso,Distancia,Tempo),
    Tempo =< Prazo.



printCircuito(Caminho,Distancia,TempoEntrega,Transporte) :-
	write('Caminho = '),writeln(Caminho),
	write('Distancia = '),write(Distancia),writeln(' km'),
	write('Tempo de entrega = '),write(TempoEntrega),writeln(' horas'),
	write('Tipo de transporte = '),writeln(Transporte),writeln('').*/
%------------------------------------------------------------------------------------------------
%------------------------------------------------------------------------------------------------
exactHeuristicCarro(Nodo,Nodo,_,0,0) :- Nodo == Nodo,!.
exactHeuristicCarro(NodoI,NodoF,Peso,EstimaKm,EstimaTmp) :-
    aprofundamentoProgress(NodoI,NodoF,_,EstimaKm),
    calculaTempo(carro,Peso,EstimaKm,EstimaTmp).

resolve_gulosa2(NodoI, NodoF, carro, Peso, Caminho) :-
    Peso =< 100,
    exactHeuristicCarro(NodoI,NodoF,Peso,EstimaKm,EstimaTmp),
    agulosaCarro2([[NodoF]/EstimaKm/EstimaTmp], Caminho/_/_, NodoI, Peso).
/*
resolve_gulosa2(NodoI, NodoF, moto, Peso, Caminho) :-
    Peso =< 20,
    estimaM(Nodo,EstimaKm,EstimaTmp),
    agulosaMoto([[Nodo]/EstimaKm/EstimaTmp],InvCaminhoD/_/_),
    reverse(InvCaminhoD,Caminho).

resolve_gulosa2(NodoI, NodoF, bicicleta, Peso, Caminho) :-
    Peso =< 5,
    estimaB(Nodo,EstimaKm,EstimaTmp),
    agulosaBic([[Nodo]/EstimaKm/EstimaTmp],InvCaminhoD/_/_),
    reverse(InvCaminhoD,Caminho).
*/

%------------------------------------------------------------------------------------------------
%---------------------------------------------  CARRO  ------------------------------------------

agulosaCarro2(Caminhos,Caminho,NodoI,_) :-
    obtem_melhorCarro(Caminhos,Caminho),
    Caminho = [NodoI|_]/_/_.
agulosaCarro2(Caminhos,SolucaoCaminho,NodoI,Peso) :-
    obtem_melhorCarro(Caminhos,MelhorCaminho),
    seleciona(MelhorCaminho,Caminhos,OutrosCaminhos),
    expande_gulosaCarro2(MelhorCaminho,ExpCaminhos,NodoI,Peso),
    append(OutrosCaminhos,ExpCaminhos,NovoCaminhos),
    agulosaCarro2(NovoCaminhos,SolucaoCaminho,NodoI,Peso).

expande_gulosaCarro2(Caminho,ExpCaminhos,NodoI,Peso) :-
    findall(NovoCaminho, (adjacenteCarro2(Caminho,NovoCaminho,NodoI,Peso)), ExpCaminhos).

adjacenteCarro2([Nodo|Caminho]/_/_, [ProxNodo,Nodo|Caminho]/EstKm/EstTmp, NodoI, Peso) :-
    move(Nodo,ProxNodo,_),
    \+member(ProxNodo,Caminho),
    exactHeuristicCarro(NodoI,ProxNodo,Peso,EstKm,EstTmp).

%------------------------------------------------------------------------------------------------
%---------------------------------------------  MOTO  ------------------------------------------
/*
agulosaMoto(Caminhos,Caminho) :-
    obtem_melhorMoto(Caminhos,Caminho),
    Caminho = [Nodo|_]/_/_,
    final(Nodo).
agulosaMoto(Caminhos,SolucaoCaminho) :-
    obtem_melhorMoto(Caminhos,MelhorCaminho),
    seleciona(MelhorCaminho,Caminhos,OutrosCaminhos),
    expande_gulosaMoto(MelhorCaminho,ExpCaminhos),
    append(OutrosCaminhos,ExpCaminhos,NovoCaminhos),
    agulosaMoto(NovoCaminhos,SolucaoCaminho).

expande_gulosaMoto(Caminho,ExpCaminhos) :-
    findall(NovoCaminho, (adjacenteMoto(Caminho,NovoCaminho)), ExpCaminhos).

adjacenteMoto([Nodo|Caminho]/_/_,[ProxNodo,Nodo|Caminho]/EstKm/EstTmp) :-
    move(Nodo,ProxNodo,_),
    \+member(ProxNodo,Caminho),
    estimaM(ProxNodo,EstKm,EstTmp).

obtem_melhorMoto([Caminho],Caminho) :- !.
obtem_melhorMoto([Caminho1/EstKm1/EstTmp1,_/EstKm2/EstTmp2|Caminhos],MelhorCaminho) :-
    Produtividade1 is 0.3 * EstKm1 + 0.7 * EstTmp1 ,Produtividade2 is 0.3 * EstKm2 + 0.7 * EstTmp2 ,
    Produtividade1 =< Produtividade2 , !,
    obtem_melhorMoto([Caminho1/EstKm1/EstTmp1|Caminhos],MelhorCaminho).
obtem_melhorMoto([_|Caminhos],MelhorCaminho) :-
    obtem_melhorMoto(Caminhos,MelhorCaminho).

%------------------------------------------------------------------------------------------------
%---------------------------------------------BICICLETA------------------------------------------

agulosaBic(Caminhos,Caminho) :-
    obtem_melhorBic(Caminhos,Caminho),
    Caminho = [Nodo|_]/_/_,
    final(Nodo).
agulosaBic(Caminhos,SolucaoCaminho) :-
    obtem_melhorBic(Caminhos,MelhorCaminho),
    seleciona(MelhorCaminho,Caminhos,OutrosCaminhos),
    expande_gulosaBic(MelhorCaminho,ExpCaminhos),
    append(OutrosCaminhos,ExpCaminhos,NovoCaminhos),
    agulosaBic(NovoCaminhos,SolucaoCaminho).

expande_gulosaBic(Caminho,ExpCaminhos) :-
    findall(NovoCaminho, (adjacenteBic(Caminho,NovoCaminho)), ExpCaminhos).

adjacenteBic([Nodo|Caminho]/_/_,[ProxNodo,Nodo|Caminho]/EstKm/EstTmp) :-
    move(Nodo,ProxNodo,_),
    \+member(ProxNodo,Caminho),
    estimaB(ProxNodo,EstKm,EstTmp).

obtem_melhorBic([Caminho],Caminho) :- !.
obtem_melhorBic([Caminho1/EstKm1/EstTmp1,_/EstKm2/EstTmp2|Caminhos],MelhorCaminho) :-
    Produtividade1 is 0.8 * EstKm1 + 0.2 * EstTmp1 ,Produtividade2 is 0.8 * EstKm2 + 0.2 * EstTmp2 ,
    Produtividade1 =< Produtividade2 , !,
    obtem_melhorBic([Caminho1/EstKm1/EstTmp1|Caminhos],MelhorCaminho).
obtem_melhorBic([_|Caminhos],MelhorCaminho) :-
    obtem_melhorBic(Caminhos,MelhorCaminho).

*/