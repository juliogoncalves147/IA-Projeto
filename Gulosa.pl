resolve_gulosa(Destino,Veiculo,Peso, CaminhoD/CustoD, CaminhoT/CustoT ) :-
    estima(Nodo,EstimaKm,EstimaTmp),
    agulosaD([[Nodo]/0/EstimaKm],InvCaminhoD/CustoD/_),
    agulosaT([[Nodo]/0/EstimaTmp],InvCaminhoT/CustoT/_),
    reverse(InvCaminhoD,CaminhoD),
    reverse(InvCaminhoT,CaminhoT).

agulosaD(Caminhos,Caminho) :-
    obtem_melhor_g(Caminhos,Caminho),
    Caminho = [Nodo|_]/_/_,
    goal(Nodo).
agulosaD(Caminhos,SolucaoCaminho) :-
    obtem_melhor_g(Caminhos,MelhorCaminho),
    seleciona(MelhorCaminho,Caminhos,OutrosCaminhos),
    expande_gulosaD(MelhorCaminho,ExpCaminhos),
    append(OutrosCaminhos,ExpCaminhos,NovoCaminhos),
    agulosaD(NovoCaminhos,SolucaoCaminho). 

agulosaT(Caminhos,Caminho) :-
    obtem_melhor_g(Caminhos,Caminho),
    Caminho = [Nodo|_]/_/_,
    goal(Nodo).
agulosaT(Caminhos,SolucaoCaminho) :-
    obtem_melhor_g(Caminhos,MelhorCaminho),
    seleciona(MelhorCaminho,Caminhos,OutrosCaminhos),
    expande_gulosaT(MelhorCaminho,ExpCaminhos),
    append(OutrosCaminhos,ExpCaminhos,NovoCaminhos),
    agulosaT(NovoCaminhos,SolucaoCaminho).    
    
obtem_melhor_g([Caminho],Caminho) :- !.
obtem_melhor_g([Caminho1/Custo1/Est1,_/_/Est2|Caminhos],MelhorCaminho) :-
    Est1 =< Est2, !,
    obtem_melhor_g([Caminho1/Custo1,Est1|Caminhos],MelhorCaminho).
obtem_melhor_g([_|Caminhos],MelhorCaminho) :-
    obtem_melhor_g(Caminhos,MelhorCaminho).

expande_gulosaD(Caminho,ExpCaminhos) :-
    findall(NovoCaminho, adjacente2D(Caminho,NovoCaminho), ExpCaminhos).

expande_gulosaT(Caminho,ExpCaminhos) :-
    findall(NovoCaminho, adjacente2T(Caminho,NovoCaminho), ExpCaminhos).

seleciona(H,[H|T],T).
seleciona(H,[X|T],[X|NewT]) :- seleciona(H,T,NewT). 

adjacente2D([Nodo|Caminho]/Custo/_,[ProxNodo,Nodo|Caminho]/NovoCusto/EstKm) :-
    move(Nodo,ProxNodo,PassoCustoKm,_),
    \+member(ProxNodo,Caminho),
    NovoCusto is Custo + PassoCustoKm,
    estima(ProxNodo,EstKm,_).

adjacente2T([Nodo|Caminho]/Custo/_,[ProxNodo,Nodo|Caminho]/NovoCusto/EstTmp) :-
    move(Nodo,ProxNodo,_,PassoCustoTmp),
    \+member(ProxNodo,Caminho),
    NovoCusto is Custo + PassoCustoTmp,
    estima(ProxNodo,_,EstTmp).

calculaTempo(carro,Peso,Distancia,Tempo) :-
    VelocidadeMedia is (25 - (0.1 * Peso)),
    Tempo is VelocidadeMedia / Distancia.
calculaTempo(bicicleta,Peso,Distancia,Tempo) :-
    VelocidadeMedia is (10 - (0.7 * Peso)),
    Tempo is VelocidadeMedia / Distancia.
calculaTempo(moto,Peso,Distancia,Tempo) :-
    VelocidadeMedia is (35 - (0.5 * Peso)),
    Tempo is VelocidadeMedia / Distancia.