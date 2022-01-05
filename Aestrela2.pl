%------------------------------------------------------------------------------------------------
%------------------------------------------------------------------------------------------------

resolve_aestrela2(NodoI,NodoF,carro,Peso,Caminho) :-
    Peso =< 100,
	exactHeuristicCarro(NodoI,NodoF,Peso,EstimaKm,EstimaTmp),
	aestrelaCarro2([[NodoF]/0/0/EstimaKm/EstimaTmp], Caminho/CustoKm/CustoTmp/_/_,NodoI,Peso).

resolve_aestrela2(NodoI,NodoF,moto,Peso,Caminho) :-
    Peso =< 20,
	exactHeuristicMoto(NodoI,NodoF,Peso,EstimaKm,EstimaTmp),
	aestrelaMoto2([[NodoF]/0/0/EstimaKm/EstimaTmp], Caminho/CustoKm/CustoTmp/_/_,NodoI,Peso),!.

resolve_aestrela2(NodoI,NodoF,bicicleta,Peso,Caminho) :-
    Peso =< 5,
	exactHeuristicBic(NodoI,NodoF,Peso,EstimaKm,EstimaTmp),
	aestrelaBic2([[NodoF]/0/0/EstimaKm/EstimaTmp], Caminho/CustoKm/CustoTmp/_/_,NodoI,Peso).

%------------------------------------------------------------------------------------------------
%---------------------------------------------  CARRO  ------------------------------------------

aestrelaCarro2(Caminhos, Caminho,NodoI,_) :-
	obtem_melhor_aeCarro(Caminhos, Caminho),
	Caminho = [NodoI|_]/_/_/_/_.
aestrelaCarro2(Caminhos, SolucaoCaminho,NodoI,Peso) :-
	obtem_melhor_aeCarro(Caminhos, MelhorCaminho),
	seleciona(MelhorCaminho, Caminhos, OutrosCaminhos),
	expande_aestrelaCarro2(MelhorCaminho, ExpCaminhos,NodoI,Peso),
	append(OutrosCaminhos, ExpCaminhos, NovoCaminhos),
    aestrelaCarro2(NovoCaminhos, SolucaoCaminho,NodoI,Peso).	

expande_aestrelaCarro2(Caminho, ExpCaminhos,NodoI,Peso) :-
	findall(NovoCaminho, adjacente_aeCarro2(Caminho,NovoCaminho,NodoI,Peso), ExpCaminhos).

adjacente_aeCarro2([Nodo|Caminho]/CustoKm/CustoTmp/_/_,[ProxNodo,Nodo|Caminho]/NovoCustoKm/NovoCustoTmp/EstKm/EstTmp,NodoI,Peso) :-
    move(Nodo,ProxNodo,PassoCustoKm),
    \+member(ProxNodo,Caminho),
    calculaTempo(carro,Peso,PassoCustoKm,PassoCustoTmp),
    NovoCustoKm is PassoCustoKm + CustoKm,
    NovoCustoTmp is PassoCustoTmp + CustoTmp,
    exactHeuristicCarro(NodoI,ProxNodo,Peso,EstKm,EstTmp).

%------------------------------------------------------------------------------------------------
%---------------------------------------------  MOTO  ------------------------------------------

aestrelaMoto2(Caminhos, Caminho,NodoI,_) :-
	obtem_melhor_aeMoto(Caminhos, Caminho),
	Caminho = [NodoI|_]/_/_/_/_.
aestrelaMoto2(Caminhos, SolucaoCaminho,NodoI,Peso) :-
	obtem_melhor_aeMoto(Caminhos, MelhorCaminho),
	seleciona(MelhorCaminho, Caminhos, OutrosCaminhos),
	expande_aestrelaMoto2(MelhorCaminho, ExpCaminhos,NodoI,Peso),
	append(OutrosCaminhos, ExpCaminhos, NovoCaminhos),
    aestrelaMoto2(NovoCaminhos, SolucaoCaminho,NodoI,Peso).	

expande_aestrelaMoto2(Caminho, ExpCaminhos,NodoI,Peso) :-
	findall(NovoCaminho, adjacente_aeMoto2(Caminho,NovoCaminho,NodoI,Peso), ExpCaminhos).

adjacente_aeMoto2([Nodo|Caminho]/CustoKm/CustoTmp/_/_,[ProxNodo,Nodo|Caminho]/NovoCustoKm/NovoCustoTmp/EstKm/EstTmp,NodoI,Peso) :-
    move(Nodo,ProxNodo,PassoCustoKm),
    \+member(ProxNodo,Caminho),
    calculaTempo(moto,Peso,PassoCustoKm,PassoCustoTmp),
    NovoCustoKm is PassoCustoKm + CustoKm,
    NovoCustoTmp is PassoCustoTmp + CustoTmp,
    exactHeuristicMoto(NodoI,ProxNodo,Peso,EstKm,EstTmp).
%------------------------------------------------------------------------------------------------
%-------------------------------------------- BICICLETA -----------------------------------------

aestrelaBic2(Caminhos, Caminho,NodoI,_) :-
	obtem_melhor_aeBic(Caminhos, Caminho),
	Caminho = [NodoI|_]/_/_/_/_.
aestrelaBic2(Caminhos, SolucaoCaminho,NodoI,Peso) :-
	obtem_melhor_aeBic(Caminhos, MelhorCaminho),
	seleciona(MelhorCaminho, Caminhos, OutrosCaminhos),
	expande_aestrelaBic2(MelhorCaminho, ExpCaminhos,NodoI,Peso),
	append(OutrosCaminhos, ExpCaminhos, NovoCaminhos),
    aestrelaBic2(NovoCaminhos, SolucaoCaminho,NodoI,Peso).	

expande_aestrelaBic2(Caminho, ExpCaminhos,NodoI,Peso) :-
	findall(NovoCaminho, adjacente_aeBic2(Caminho,NovoCaminho,NodoI,Peso), ExpCaminhos).

adjacente_aeBic2([Nodo|Caminho]/CustoKm/CustoTmp/_/_,[ProxNodo,Nodo|Caminho]/NovoCustoKm/NovoCustoTmp/EstKm/EstTmp,NodoI,Peso) :-
    move(Nodo,ProxNodo,PassoCustoKm),
    \+member(ProxNodo,Caminho),
    calculaTempo(bicicleta,Peso,PassoCustoKm,PassoCustoTmp),
    NovoCustoKm is PassoCustoKm + CustoKm,
    NovoCustoTmp is PassoCustoTmp + CustoTmp,
    exactHeuristicBic(NodoI,ProxNodo,Peso,EstKm,EstTmp).
