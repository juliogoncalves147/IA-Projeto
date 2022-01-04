exactHeuristicCarro(Nodo,Nodo,_,0,0) :- Nodo == Nodo,!.
exactHeuristicCarro(NodoI,NodoF,Peso,EstimaKm,EstimaTmp) :-
    aprofundamentoProgress(NodoI,NodoF,_,EstimaKm),
    calculaTempo(carro,Peso,EstimaKm,EstimaTmp).

exactHeuristicMoto(Nodo,Nodo,_,0,0) :- Nodo == Nodo,!.
exactHeuristicMoto(NodoI,NodoF,Peso,EstimaKm,EstimaTmp) :-
    aprofundamentoProgress(NodoI,NodoF,_,EstimaKm),
    calculaTempo(moto,Peso,EstimaKm,EstimaTmp).

exactHeuristicBic(Nodo,Nodo,_,0,0) :- Nodo == Nodo,!.
exactHeuristicBic(NodoI,NodoF,Peso,EstimaKm,EstimaTmp) :-
    aprofundamentoProgress(NodoI,NodoF,_,EstimaKm),
    calculaTempo(bicicleta,Peso,EstimaKm,EstimaTmp).

%------------------------------------------------------------------------------------------------
%------------------------------------------------------------------------------------------------

resolve_gulosa2(NodoI, NodoF, carro, Peso, Caminho) :-
    Peso =< 100,
    exactHeuristicCarro(NodoI,NodoF,Peso,EstimaKm,EstimaTmp),
    agulosaCarro2([[NodoF]/EstimaKm/EstimaTmp], Caminho/_/_, NodoI, Peso).

resolve_gulosa2(NodoI, NodoF, moto, Peso, Caminho) :-
    Peso =< 20,
    exactHeuristicMoto(NodoI,NodoF,Peso,EstimaKm,EstimaTmp),
    agulosaMoto2([[NodoF]/EstimaKm/EstimaTmp], Caminho/_/_, NodoI, Peso).

resolve_gulosa2(NodoI, NodoF, bicicleta, Peso, Caminho) :-
    Peso =< 5,
    exactHeuristicBic(NodoI,NodoF,Peso,EstimaKm,EstimaTmp),
    agulosaBic2([[NodoF]/EstimaKm/EstimaTmp], Caminho/_/_, NodoI, Peso).


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

agulosaMoto2(Caminhos,Caminho,NodoI,_) :-
    obtem_melhorMoto(Caminhos,Caminho),
    Caminho = [NodoI|_]/_/_.
agulosaMoto2(Caminhos,SolucaoCaminho,NodoI,Peso) :-
    obtem_melhorMoto(Caminhos,MelhorCaminho),
    seleciona(MelhorCaminho,Caminhos,OutrosCaminhos),
    expande_gulosaMoto2(MelhorCaminho,ExpCaminhos,NodoI,Peso),
    append(OutrosCaminhos,ExpCaminhos,NovoCaminhos),
    agulosaMoto2(NovoCaminhos,SolucaoCaminho,NodoI,Peso).

expande_gulosaMoto2(Caminho,ExpCaminhos,NodoI,Peso) :-
    findall(NovoCaminho, (adjacenteMoto2(Caminho,NovoCaminho,NodoI,Peso)), ExpCaminhos).

adjacenteMoto2([Nodo|Caminho]/_/_, [ProxNodo,Nodo|Caminho]/EstKm/EstTmp, NodoI, Peso) :-
    move(Nodo,ProxNodo,_),
    \+member(ProxNodo,Caminho),
    exactHeuristicMoto(NodoI,ProxNodo,Peso,EstKm,EstTmp).

%------------------------------------------------------------------------------------------------
%---------------------------------------------BICICLETA------------------------------------------

agulosaBic2(Caminhos,Caminho,NodoI,_) :-
    obtem_melhorBic(Caminhos,Caminho),
    Caminho = [NodoI|_]/_/_.
agulosaBic2(Caminhos,SolucaoCaminho,NodoI,Peso) :-
    obtem_melhorBic(Caminhos,MelhorCaminho),
    seleciona(MelhorCaminho,Caminhos,OutrosCaminhos),
    expande_gulosaBic2(MelhorCaminho,ExpCaminhos,NodoI,Peso),
    append(OutrosCaminhos,ExpCaminhos,NovoCaminhos),
    agulosaBic2(NovoCaminhos,SolucaoCaminho,NodoI,Peso).

expande_gulosaBic2(Caminho,ExpCaminhos,NodoI,Peso) :-
    findall(NovoCaminho, (adjacenteBic2(Caminho,NovoCaminho,NodoI,Peso)), ExpCaminhos).

adjacenteBic2([Nodo|Caminho]/_/_, [ProxNodo,Nodo|Caminho]/EstKm/EstTmp, NodoI, Peso) :-
    move(Nodo,ProxNodo,_),
    \+member(ProxNodo,Caminho),
    exactHeuristicBic(NodoI,ProxNodo,Peso,EstKm,EstTmp).