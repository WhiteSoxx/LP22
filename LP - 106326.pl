% Guilherme Filipe, 106326
:- set_prolog_flag(answer_write_options,[max_depth(0)]).
:- [dados], [keywords].


/* eventosSemSalas(EventosSemSala)
Devolve uma lista com os IDs de eventos sem sala de dados.pl.*/
eventosSemSalas(EventosSemSala):- findall(ID,evento(ID,_,_,_,semSala),EventosSemSala).


/* eventosSemSalasDiaSemana(DiaDaSemana,EventosSemSala)
Devolve uma lista com os IDs de eventos sem sala de dados.pl no DiaDaSemana.*/
eventosSemSalasDiaSemana(DiaDaSemana,EventosSemSala):- findall(ID,(horario(ID,DiaDaSemana,_,_,_,_),
        evento(ID,_,_,_,semSala)),EventosSemSala).


/* Predicado auxiliar, estaNoPeriodo(ID,P)
Devolve True se o ID do evento fornecido ocorre no periodo P ou no seu respetivo semestre.*/
estaNoPeriodo(ID,P) :- member(P,[p1,p2]), horario(ID,_,_,_,_,p1_2);horario(ID,_,_,_,_,P).
estaNoPeriodo(ID,P) :- member(P,[p3,p4]), horario(ID,_,_,_,_,p3_4);horario(ID,_,_,_,_,P).

/* eventosSemSalasPeriodo(ListaPeriodos,EventosSemSala)
Devolve uma lista com os IDs de eventos sem sala de dados.pl no(s) periodo(s) fornecido(s).*/
eventosSemSalasPeriodo([],[]).
eventosSemSalasPeriodo([P|R],EventosSemSala):-
        findall(ID,(evento(ID,_,_,_,semSala),estaNoPeriodo(ID,P)),Q),
        eventosSemSalasPeriodo(R,S), append(Q,S,T), sort(T,EventosSemSala).


/* organizaEventos(Eventos,Periodo,EventosNoPeriodo)
Devolve uma lista com os IDs de eventos de dados.pl que decorrem no periodo. 
Dividido em dois casos.*/
organizaEventos(Eventos,Periodo,EventosNoPeriodo) :- organizaEventos(Eventos,Periodo,EventosAux,[]),
        sort(EventosAux,EventosNoPeriodo).
organizaEventos([],_,Aux,Aux).
organizaEventos([P|R],Periodo,EventosNoPeriodo,Aux) :- estaNoPeriodo(P,Periodo),
        organizaEventos(R,Periodo,EventosNoPeriodo,[P|Aux]). %Adicionar a lista.
organizaEventos([P|R],Periodo,EventosNoPeriodo,EventosAux) :- \+ estaNoPeriodo(P,Periodo),
        organizaEventos(R,Periodo,EventosNoPeriodo,EventosAux). %Mantem.


/* eventosMenoresQue(Duracao,ListaEventosMenoresQue)
Devolve uma lista com os IDs de eventos de dados.pl que decorrem no periodo.*/
eventosMenoresQue(Duracao,ListaEventosMenoresQue) :-
        findall(ID,(horario(ID,_,_,_,Avaliar,_),Avaliar=<Duracao),ListaEventosMenoresQue).


/* eventosMenoresQueBool(ID,Duracao)
Devolve True se o evento ID tem duracao menor ou igual a Duracao.*/
eventosMenoresQueBool(ID,Duracao) :- horario(ID,_,_,_,Avaliar,_), Avaliar=<Duracao.


/* procuraDisciplinas(Curso,ListaDisciplinas)
Devolve uma lista com as disciplinas de Curso, ordenadas.*/
procuraDisciplinas(Curso,ListaDisciplinas) :-
        findall(Disciplina,(turno(ID,Curso,_,_),evento(ID,Disciplina,_,_,_)),ListaAux),
        sort(ListaAux,ListaDisciplinas).


/* Predicados auxiliares, semestre1(Disciplina,Curso) e semestre2(Disciplina,Curso)
Devolvem True se a Disciplina de Curso pertencer ao 1o ou 2o semestres.*/
semestre1(Disciplina,Curso) :- evento(ID,Disciplina,_,_,_), turno(ID,Curso,_,_),
        horario(ID,_,_,_,_,Periodo), member(Periodo,[p1,p2,p1_2]). %tem de cumprir todas as condicoes
semestre2(Disciplina,Curso) :- evento(ID,Disciplina,_,_,_), turno(ID,Curso,_,_),
        horario(ID,_,_,_,_,Periodo), member(Periodo,[p3,p4,p3_4]).


/* organizaDisciplinas(ListaDisciplinas,Curso,Semestres)
Semestres representa uma lista de duas listas, uma para 
cada semestre, com as disciplinas de curso, ordenadas.*/
organizaDisciplinas(ListaDisciplinas,Curso,Semestres) :-
        organizaDisciplinas(ListaDisciplinas,Curso,Semestres,[],[]). % e util ter duas listas, uma para cada semestre
organizaDisciplinas([],_,[L1|[L2]],L1,L2).
organizaDisciplinas([P|R],Curso,Semestres,L1,L2) :- semestre1(P,Curso), Aux = [P|L1],
        sort(Aux,Aux2), organizaDisciplinas(R,Curso,Semestres,Aux2,L2),!.
organizaDisciplinas([P|R],Curso,Semestres,L1,L2) :- semestre2(P,Curso), Aux = [P|L2],
        sort(Aux,Aux2), organizaDisciplinas(R,Curso,Semestres,L1,Aux2),!.


/* Predicado auxiliar,somaHorasLista(ListaIDs,Soma)
Soma representa a soma das horas de todos os eventos de ListaIDs.*/
somaHorasLista([],0).
somaHorasLista([P|R],Soma) :- somaHorasLista(R,Soma1), horario(P,_,_,_,Horas,_),
        Soma is Soma1 + Horas. % acumulador.

/* horasCurso(Periodo,Curso,Ano,TotalHoras)
TotalHoras representa o total de horas de todos os eventos de Curso no Ano e Periodo.*/
horasCurso(Periodo,Curso,Ano,TotalHoras) :- member(Periodo,[p1,p2]),!, %dividimos em dois casos por causa do p1_2
        findall(ID,(turno(ID,Curso,Ano,_),horario(ID,_,_,_,_,Periodo)),ListaEventosPeriodo),
        findall(ID,(turno(ID,Curso,Ano,_),horario(ID,_,_,_,_,p1_2)),ListaEventosSemestre),
        append(ListaEventosPeriodo,ListaEventosSemestre,ListaPorOrdenar), %duas listas juntas simplifica a obtencao de eventos
        sort(ListaPorOrdenar,ListaCompleta),
        somaHorasLista(ListaCompleta,Soma), TotalHoras is Soma.
horasCurso(Periodo,Curso,Ano,TotalHoras) :- member(Periodo,[p3,p4]),!,
        findall(ID,(turno(ID,Curso,Ano,_),horario(ID,_,_,_,_,Periodo)),ListaEventosPeriodo),
        findall(ID,(turno(ID,Curso,Ano,_),horario(ID,_,_,_,_,p3_4)),ListaEventosSemestre),
        append(ListaEventosPeriodo,ListaEventosSemestre,ListaPorOrdenar),
        sort(ListaPorOrdenar,ListaCompleta),
        somaHorasLista(ListaCompleta,Soma), TotalHoras is Soma.


/* Predicado Auxiliar, criaTuplo(Curso,[Ano,Periodo],Tuplo)
Serve o unico proposito de converter informacao num tuplo.*/
criaTuplo(Curso,[Ano,Periodo],Tuplo) :- horasCurso(Periodo,Curso,Ano,TotalHoras),
        Tuplo = (Ano,Periodo,TotalHoras).

/* evolucaoHorasCurso(Curso,Evolucao)
Evolucao representa uma lista de tuplos (Ano,Periodo,TotalHoras) de Curso.*/
evolucaoHorasCurso(Curso,Evolucao) :-
        ListaPeriodos = [[1,p1],[1,p2],[1,p3],[1,p4],[2,p1],[2,p2],[2,p3],[2,p4],[3,p1],[3,p2],[3,p3],[3,p4]],
        evolucaoHorasCurso(Curso,Evolucao,ListaPeriodos). %o predicado precisava de informacao ano/periodo
evolucaoHorasCurso(Curso,Evolucao,ListaPeriodos) :- maplist(criaTuplo(Curso),ListaPeriodos,Evolucao).


/* ocupaSlot(HoraInicioDada,HoraFimDada,HoraInicioEvento,HoraFimEvento,Horas)
Horas representa o tamanho do 'slot' ocupado da sala durante o periodo de tempo dado.
Devolve True se esse tamanho nao for nulo.*/
ocupaSlot(HoraInicioDada,HoraFimDada,HoraInicioEvento,HoraFimEvento,Horas) :-
        Horas is min(HoraFimDada,HoraFimEvento) - max(HoraInicioDada,HoraInicioEvento),
        Horas > 0. %tem que estar no slot


/*Predicado auxiliar, listaEventosNasSalas(Periodo,ListaSalas,DiaSemana,ListaEventos)
ListaEventos representa uma lista com todos os IDs de eventos em cada sala,
no Periodo e DiaSemana.*/
listaEventosNasSalas(_,[],_,[]). 
listaEventosNasSalas(Periodo,[P|R],DiaSemana,ListaEventos) :- member(Periodo,[p1,p2]),!,
        findall(ID,(horario(ID,DiaSemana,_,_,_,Periodo),evento(ID,_,_,_,P)),ListaEventosPeriodo),
        findall(ID,(horario(ID,DiaSemana,_,_,_,p1_2),evento(ID,_,_,_,P)),ListaEventosSemestre),
        append(ListaEventosPeriodo,ListaEventosSemestre,ListaCompleta),
        listaEventosNasSalas(Periodo,R,DiaSemana,ListaEventos1),
        append(ListaCompleta,ListaEventos1,ListaEventos).
listaEventosNasSalas(Periodo,[P|R],DiaSemana,ListaEventos) :- member(Periodo,[p3,p4]),!,
        findall(ID,(horario(ID,DiaSemana,_,_,_,Periodo),evento(ID,_,_,_,P)),ListaEventosPeriodo),
        findall(ID,(horario(ID,DiaSemana,_,_,_,p3_4),evento(ID,_,_,_,P)),ListaEventosSemestre),
        append(ListaEventosPeriodo,ListaEventosSemestre,ListaCompleta),
        listaEventosNasSalas(Periodo,R,DiaSemana,ListaEventos1),
        append(ListaCompleta,ListaEventos1,ListaEventos).

/*Predicado auxiliar, somaHorasLista_v2(ListaEventos,HoraInicioDada,HoraFimDada,Soma)
Soma representa a soma das horas de todos os eventos na lista, durante o periodo de tempo dado.*/
somaHorasLista_v2([],_,_,0). 
somaHorasLista_v2([P|R],HoraInicioDada,HoraFimDada,Soma) :- 
        horario(P,_,HoraInicioEvento,HoraFimEvento,_,_),
        ocupaSlot(HoraInicioDada,HoraFimDada,HoraInicioEvento,HoraFimEvento,Horas),!,
        somaHorasLista_v2(R,HoraInicioDada,HoraFimDada,Soma1),
        Soma is Soma1 + Horas.
somaHorasLista_v2([P|R],HoraInicioDada,HoraFimDada,Soma) :-
        horario(P,_,HoraInicioEvento,HoraFimEvento,_,_),
        \+ ocupaSlot(HoraInicioDada,HoraFimDada,HoraInicioEvento,HoraFimEvento,_),!,
        somaHorasLista_v2(R,HoraInicioDada,HoraFimDada,Soma). %nao ha soma porque o resultado seria 0

/* numHorasOcupadas(Periodo,TipoSala,DiaSemana,HoraInicio,HoraFim,SomaHoras)
SomaHoras representa a soma das horas em que cada TipoSala esta ocupado no DiaSemana,
Periodo, e periodo de tempo definido.*/
numHorasOcupadas(Periodo,TipoSala,DiaSemana,HoraInicio,HoraFim,SomaHoras) :-
        salas(TipoSala,Salas),
        listaEventosNasSalas(Periodo,Salas,DiaSemana,ListaEventos),
        somaHorasLista_v2(ListaEventos,HoraInicio,HoraFim,SomaHoras).


/* ocupacaoMax(TipoSala,HoraInicio,HoraFim,Max)
Max representa o maximo de horas em que TipoSala 
se encontra ocupado no periodo de tempo definido.*/
ocupacaoMax(TipoSala,HoraInicio,HoraFim,Max) :- salas(TipoSala,Salas),
        length(Salas,NumSalas), Max is (HoraFim - HoraInicio) * NumSalas.


/* percentagem(SomaHoras,Max,Percentagem)
Percentagem representa a percentagem de ocupacao, tendo em conta a ocupacao maxima e atual.*/
percentagem(SomaHoras,Max,Percentagem) :- Percentagem is (SomaHoras / Max) * 100.


/* ocupacaoCritica(HoraInicio,HoraFim,Treshold,Resultados)
Resultados representa uma lista de tuplos com os casos criticos de ocupacao,
definidos quando a percentagem de ocupacao e superior a Treshold no periodo de tempo
definido, e em qualquer TipoSala, Periodo, ou DiaSemana.*/
ocupacaoCritica(HoraInicio,HoraFim,Treshold,Resultados) :-
        %primeiro vamos obter os dados que precisamos para testar combinacoes
        setof(DiaDaSemana,A^B^C^D^E^horario(A,DiaDaSemana,B,C,D,E),DiasSemana),
        setof(Periodo,A^B^C^D^E^(horario(A,B,C,D,E,Periodo),member(Periodo,[p1,p2,p3,p4])),Periodos),
        setof(TipoSala,A^salas(TipoSala,A),TiposSala), 
        %extrair a informacao em vez de a fornecer contribui para a abstracao procedimental. o programa
        %nao deve vir formatado para saber logo que tipos de sala existem! dito isto,
        %agora, testamos as combinacoes possiveis.
        findall(casosCriticos(DiaSemana,TipoSala,Percentagem),
        (member(DiaSemana,DiasSemana),member(Periodo,Periodos),member(TipoSala,TiposSala),
        numHorasOcupadas(Periodo,TipoSala,DiaSemana,HoraInicio,HoraFim,SomaHoras),
        ocupacaoMax(TipoSala,HoraInicio,HoraFim,Max),
        percentagem(SomaHoras,Max,PercentagemAux),PercentagemAux > Treshold, 
        ceiling(PercentagemAux,Percentagem)),ResultadosAux),
        sort(ResultadosAux,Resultados).



%Mesa

%Restricoes
cab1(NomePessoa,OcupacaoMesa) :-
        OcupacaoMesa = [[_,_,_],[NomePessoa,_],[_,_,_]].
cab2(NomePessoa,OcupacaoMesa) :-
        OcupacaoMesa = [[_,_,_],[_,NomePessoa],[_,_,_]].

honra(NomePessoa1,NomePessoa2,OcupacaoMesa) :-
        OcupacaoMesa = [[_,_,_],[NomePessoa1,_],[NomePessoa2,_,_]];
        OcupacaoMesa = [[_,_,NomePessoa2],[_,NomePessoa1],[_,_,_]];
        OcupacaoMesa = [[_,_,_],[NomePessoa2,_],[NomePessoa1,_,_]];
        OcupacaoMesa = [[_,_,NomePessoa1],[_,NomePessoa2],[_,_,_]].

lado(NomePessoa1,NomePessoa2,OcupacaoMesa) :-
        OcupacaoMesa = [[NomePessoa1,NomePessoa2,_],[_,_],[_,_,_]];
        OcupacaoMesa = [[_,NomePessoa1,NomePessoa2],[_,_],[_,_,_]];
        OcupacaoMesa = [[_,_,_],[_,_],[NomePessoa1,NomePessoa2,_]];
        OcupacaoMesa = [[_,_,_],[_,_],[_,NomePessoa1,NomePessoa2]];
        OcupacaoMesa = [[NomePessoa2,NomePessoa1,_],[_,_],[_,_,_]];
        OcupacaoMesa = [[_,NomePessoa2,NomePessoa1],[_,_],[_,_,_]];
        OcupacaoMesa = [[_,_,_],[_,_],[NomePessoa2,NomePessoa1,_]];
        OcupacaoMesa = [[_,_,_],[_,_],[_,NomePessoa2,NomePessoa1]].
naoLado(NomePessoa1,NomePessoa2,OcupacaoMesa) :-  
        \+ lado(NomePessoa1,NomePessoa2,OcupacaoMesa).

frente(NomePessoa1,NomePessoa2,OcupacaoMesa) :- 
        OcupacaoMesa = [[NomePessoa1,_,_],[_,_],[NomePessoa2,_,_]];
        OcupacaoMesa = [[_,NomePessoa1,_],[_,_],[_,NomePessoa2,_]];
        OcupacaoMesa = [[_,_,NomePessoa1],[_,_],[_,_,NomePessoa2]];
        OcupacaoMesa = [[NomePessoa2,_,_],[_,_],[NomePessoa1,_,_]];
        OcupacaoMesa = [[_,NomePessoa2,_],[_,_],[_,NomePessoa1,_]];
        OcupacaoMesa = [[_,_,NomePessoa2],[_,_],[_,_,NomePessoa1]].
naoFrente(NomePessoa1,NomePessoa2,OcupacaoMesa) :- 
        \+ frente(NomePessoa1,NomePessoa2,OcupacaoMesa).

/* Predicados Auxiliares, estaSentado(ListaPessoas,OcupacaoMesa),
estaoSentados(ListaPessoas,OcupacaoMesa,OcupacaoMesaAux)
Devolve True se todas as pessoas em ListaPessoas se encontram em OcupacaoMesa.*/
estaoSentados(ListaPessoas,OcupacaoMesa) :- flatten(OcupacaoMesa,OcupacaoMesaAux),!,
        estaoSentados(ListaPessoas,OcupacaoMesa,OcupacaoMesaAux).
estaoSentados([],_,_).
estaoSentados([P|R],_,OcupacaoMesaAux) :-
        member(P,OcupacaoMesaAux), estaoSentados(R,_,OcupacaoMesaAux).

/* Predicado Auxiliar, aplicaRestricoes(ListaRestricoes,OcupacaoMesa)
Verifica se OcupacaoMesa cumpre todas as restricoes fornecidas.*/
aplicaRestricoes([],_).
aplicaRestricoes([P|R],OcupacaoMesa) :-
        call(P,OcupacaoMesa),!,aplicaRestricoes(R,OcupacaoMesa).

/* ocupacaoMesa(ListaPessoas,ListaRestricoes,OcupacaoMesa)
OcupacaoMesa representa a lista [[_,_,_],[_,_],[_,_,_]] com todos os espacos preenchidos
pelos elementos de ListaPessoas, tendo em conta as restricoes em ListaRestricoes.*/
ocupacaoMesa(ListaPessoas,ListaRestricoes,OcupacaoMesa) :- OcupacaoMesa = [[_,_,_],[_,_],[_,_,_]],!,
        estaoSentados(ListaPessoas,OcupacaoMesa), 
        aplicaRestricoes(ListaRestricoes,OcupacaoMesa).