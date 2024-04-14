% 7.17.1

substitui_f(T_c, Novo_F, Novo_T_c) :-
    T_c =.. [_ | Args],
    Novo_T_c =.. [Novo_F | Args].

substitui_arg(T_c,Arg, Novo_Arg, Novo_T_c) :-
    T_c =.. [F | Args],
    substitui_el_lst(Args, Arg, Novo_Arg, Novos_Args),
    Novo_T_c =.. [F|Novos_Args].

%
substitui_el_lst([], _,_,[]) :- !.
substitui_el_lst([El|R], El, Novo_El, [Novo_El | Lst]) :-
    !,
    substitui_el_lst(R, El, Novo_El, Lst).

substitui_el_lst([P|R], El, Novo_El, [P | Lst]) :-
    substitui_el_lst(R, El, Novo_El, Lst).

% 7.17.5

par(X) :- X mod 2 =:= 0.

quantos(_, [], 0) :- !.
quantos(Predicado, [P|R], N) :-
    Lit =.. [Predicado, P],
    Lit, !,
    quantos(Predicado, R, N1),
    N is N1 +1.

quantos(Predicado, [_|R], N) :-
    quantos(Predicado, R, N).

% 7.17.6

transforma(_, [], []) :- !.
transforma(Tr, [P | R],[NP | NR]) :-
    Lit =.. [Tr, P, NP],
    Lit, !,
    transforma(Tr, R, NR).

soma_1(X, Y) :-
    Y is X+1.

% 7.17.7

filtra_inc([],_, []) :- !.
filtra_inc([P|R],Tst, [P|R2]) :-
    Lit =.. [Tst, P],
    Lit,!,
    filtra_inc(R, Tst, R2).

filtra_inc([_|R], Tst, R2) :-
    filtra_inc(R, Tst, R2).

% 7.17.4

algum(Pred, [P | _]) :-
    Lit =.. [Pred, P],
    Lit, !.

algum(Pred, [_ | R]) :-
    algum(Pred, R).
