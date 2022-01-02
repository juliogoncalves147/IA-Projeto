%------------------------------------------------------------------------------------------------
%------------------------------------------------------------------------------------------------

resolve_gulosa(Nodo, carro, Peso, CaminhoD/CustoD, CaminhoT/CustoT ) :-
    Peso < 100,
    estimaC(Nodo,EstimaKm,EstimaTmp),
    agulosaDcarro([[Nodo]/0/EstimaKm],InvCaminhoD/CustoD/_),
    agulosaTcarro([[Nodo]/0/EstimaTmp],InvCaminhoT/CustoT/_,Peso),
    reverse(InvCaminhoD,CaminhoD),
    reverse(InvCaminhoT,CaminhoT).
%resolve_gulosa(Nodo, moto, Peso, CaminhoD/CustoD, CaminhoT/CustoT ) :-
%    Peso < 20,
%    estimaM(Nodo,EstimaKm,EstimaTmp),
%    agulosaD([[Nodo]/0/EstimaKm],InvCaminhoD/CustoD/_),
%    agulosaT([[Nodo]/0/EstimaTmp],InvCaminhoT/CustoT/_),
%    reverse(InvCaminhoD,CaminhoD),
%    reverse(InvCaminhoT,CaminhoT).
%resolve_gulosa(Nodo, bicicleta, Peso, CaminhoD/CustoD, CaminhoT/CustoT ) :-
%    Peso < 5,
%    estimaB(Nodo,EstimaKm,EstimaTmp),
%    agulosaD([[Nodo]/0/EstimaKm],InvCaminhoD/CustoD/_),
%    agulosaT([[Nodo]/0/EstimaTmp],InvCaminhoT/CustoT/_),
%    reverse(InvCaminhoD,CaminhoD),
%    reverse(InvCaminhoT,CaminhoT).

%------------------------------------------------------------------------------------------------
%---------------------------------------------  CARRO  ------------------------------------------

agulosaDcarro(Caminhos,Caminho) :-
    obtem_melhor_g(Caminhos,Caminho),
    Caminho = [Nodo|_]/_/_,
    final(Nodo).
agulosaDcarro(Caminhos,SolucaoCaminho) :-
    obtem_melhor_g(Caminhos,MelhorCaminho),
    seleciona(MelhorCaminho,Caminhos,OutrosCaminhos),
    expande_gulosaDcarro(MelhorCaminho,ExpCaminhos),
    append(OutrosCaminhos,ExpCaminhos,NovoCaminhos),
    agulosaDcarro(NovoCaminhos,SolucaoCaminho). 

agulosaTcarro(Caminhos,Caminho,_) :-
    obtem_melhor_g(Caminhos,Caminho),
    Caminho = [Nodo|_]/_/_,
    final(Nodo).
agulosaTcarro(Caminhos,SolucaoCaminho,Peso) :-
    obtem_melhor_g(Caminhos,MelhorCaminho),
    seleciona(MelhorCaminho,Caminhos,OutrosCaminhos),
    expande_gulosaTcarro(MelhorCaminho,ExpCaminhos,Peso),
    append(OutrosCaminhos,ExpCaminhos,NovoCaminhos),
    agulosaTcarro(NovoCaminhos,SolucaoCaminho,Peso).    
    
expande_gulosaDcarro(Caminho,ExpCaminhos) :-
    findall(NovoCaminho, (adjacente2Dcarro(Caminho,NovoCaminho)) , ExpCaminhos).

expande_gulosaTcarro(Caminho,ExpCaminhos,Peso) :-
    findall(NovoCaminho, (adjacente2Tcarro(Caminho,NovoCaminho,Peso)) , ExpCaminhos).

adjacente2Dcarro([Nodo|Caminho]/Custo/_,[ProxNodo,Nodo|Caminho]/NovoCusto/EstKm) :-
    move(Nodo,ProxNodo,PassoCustoKm),
    \+member(ProxNodo,Caminho),
    NovoCusto is Custo + PassoCustoKm,
    estimaC(ProxNodo,EstKm,_).

adjacente2Tcarro([Nodo|Caminho]/Custo/_,[ProxNodo,Nodo|Caminho]/NovoCusto/EstTmp,Peso) :-
    move(Nodo,ProxNodo,PassoCustoKm),
    calculaTempo(carro,Peso,PassoCustoKm,PassoCustoTmp),
    \+member(ProxNodo,Caminho),
    NovoCusto is Custo + PassoCustoTmp,
    estimaC(ProxNodo,_,EstTmp).

%------------------------------------------------------------------------------------------------
%---------------------------------------------  MOTO  ------------------------------------------

agulosaDmoto(Caminhos,Caminho) :-
    obtem_melhor_g(Caminhos,Caminho),
    Caminho = [Nodo|_]/_/_,
    final(Nodo).
agulosaDmoto(Caminhos,SolucaoCaminho) :-
    obtem_melhor_g(Caminhos,MelhorCaminho),
    seleciona(MelhorCaminho,Caminhos,OutrosCaminhos),
    expande_gulosaDmoto(MelhorCaminho,ExpCaminhos),
    append(OutrosCaminhos,ExpCaminhos,NovoCaminhos),
    agulosaDmoto(NovoCaminhos,SolucaoCaminho). 

agulosaTmoto(Caminhos,Caminho,_) :-
    obtem_melhor_g(Caminhos,Caminho),
    Caminho = [Nodo|_]/_/_,
    final(Nodo).
agulosaTmoto(Caminhos,SolucaoCaminho,Peso) :-
    obtem_melhor_g(Caminhos,MelhorCaminho),
    seleciona(MelhorCaminho,Caminhos,OutrosCaminhos),
    expande_gulosaTmoto(MelhorCaminho,ExpCaminhos,Peso),
    append(OutrosCaminhos,ExpCaminhos,NovoCaminhos),
    agulosaTmoto(NovoCaminhos,SolucaoCaminho,Peso).    
    
expande_gulosaDmoto(Caminho,ExpCaminhos) :-
    findall(NovoCaminho, (adjacente2Dmoto(Caminho,NovoCaminho)) , ExpCaminhos).

expande_gulosaTmoto(Caminho,ExpCaminhos,Peso) :-
    findall(NovoCaminho, (adjacente2Tmoto(Caminho,NovoCaminho,Peso)) , ExpCaminhos).

adjacente2Dcarro([Nodo|Caminho]/Custo/_,[ProxNodo,Nodo|Caminho]/NovoCusto/EstKm) :-
    move(Nodo,ProxNodo,PassoCustoKm),
    \+member(ProxNodo,Caminho),
    NovoCusto is Custo + PassoCustoKm,
    estimaC(ProxNodo,EstKm,_).

adjacente2Tcarro([Nodo|Caminho]/Custo/_,[ProxNodo,Nodo|Caminho]/NovoCusto/EstTmp,Peso) :-
    move(Nodo,ProxNodo,PassoCustoKm),
    calculaTempo(carro,Peso,PassoCustoKm,PassoCustoTmp),
    \+member(ProxNodo,Caminho),
    NovoCusto is Custo + PassoCustoTmp,
    estimaC(ProxNodo,_,EstTmp).

%------------------------------------------------------------------------------------------------
%--------------------------------------      GLOBAIS    -----------------------------------------
obtem_melhor_g([Caminho],Caminho) :- !.
obtem_melhor_g([Caminho1/Custo1/Est1,_/_/Est2|Caminhos],MelhorCaminho) :-
    Est1 =< Est2, !,
    obtem_melhor_g([Caminho1/Custo1/Est1|Caminhos],MelhorCaminho).
obtem_melhor_g([_|Caminhos],MelhorCaminho) :-
    obtem_melhor_g(Caminhos,MelhorCaminho).

%seleciona(_,[],[]).
seleciona(H,[H|T],T).
seleciona(H,[X|T],[X|NewT]) :- seleciona(H,T,NewT). 

calculaTempo(carro,Peso,Distancia,Tempo) :-
    VelocidadeMedia is (25 - (0.1 * Peso)),
    Tempo is (Distancia / VelocidadeMedia).
calculaTempo(bicicleta,Peso,Distancia,Tempo) :-
    VelocidadeMedia is (10 - (0.7 * Peso)),
    Tempo is (Distancia / VelocidadeMedia).
calculaTempo(moto,Peso,Distancia,Tempo) :-
    VelocidadeMedia is (35 - (0.5 * Peso)),
    Tempo is (Distancia / VelocidadeMedia).

move(X,Y,Custo) :- aresta(X,Y,Custo).
move(X,Y,Custo) :- aresta(Y,X,Custo).