% --HYPER--

:- op(500, xfx, :).

not(A) :-
  A, !, fail
  ; true.

once(A) :-
  A, !.

conc([ ], L, L).
conc([ X | L1 ], L2, [ X | L3 ]) :- 
  conc(L1, L2, L3).

member(X, [X | Xs]).
member(X, [Y | Ys]) :-
  member(X, Ys).

del(X, [X | Xs], Xs).
del(X, [Y | Ys], [Z | Zs]) :-
  del(X, Ys, Zs).

length(L, N) :-
  length(L, 0, N).
length([], N, N).
length([_ | L], N0, N) :-
  N1 is N0+1,
  length(L, N1, N).

%--

induce(Hyp) :-
  init_counts, 
  !,
  start_hyps(Hyps),
  search(Hyps, _:Hyp).

search([Hyp | Hyps], Hyp) :-
  show_counts,
  Hyp = 0:H,
  complete(H).

search([C0:H0 | Hyps0], H) :-
  write('Refining hypo-s with cost '), write(C0),
  write(:), nl, show_hyp(H0), nl,
  all_refinements(H0m NewHs),
  add_hyps(NewHs, Hyps0, Hyps), !, add1(refined),
  search(Hyps, H).
 
all_refinements(H0, Hyps) :-
  findall( C:H, (refine_hyp(H0, H),
                once((add1(generated),
                          complete(H),
                          add1(complete),
                          eval(H,C)
                 ))),
                 Hyps).

add_hyps(Hyps1, Hyps2, Hyps) :-
  mergesort(Hyps1, OrderHyps1, Hyps),
  merge(Hyps2, OrderHyps1, Hyps).

complete(Hyp) :-
  not( ex(P),
      once (prove(P, Hyp, A)),
      A \== yes).


eval(Hyp, Cost) :-
  size(Hyp, S),
  convers_neg(Hyp, N),
  ( N = 0, !, Cost is 0 ; Cost is S + 10 * N).

size([], 0).
size([Cs0 / Vs0 | RestH], Size) :-
  length(Cs0, L0),
  length(Vs0, N0),
  size(RestH, SizeRest),
  Size is 10 * L0 + N0 + SizeRest.

covers_neg(Hyp, N) :-
  findall( 1, (nex(E), once(prove(E, Hyp, A)), A \== no), L),
  length(L, N).

unsatisfiable([Head | Body], Hyp) :-
  once(prove(Body, Hyp, A)), A = no.

start_hyps(Hyps) :-
  max_clauses(M),
  setof( C:H,
    (start_hyp(H, M), add1(generated),
    complete(H), add1(complete), eval(H, C)),
  Hyps).

start_hyp([], _).
start_hyp([C | Cs], M) :-
  M > 0, M1 is M-1,
  start_clause(C),
  start_hyp(Cs, M1).

refine_hyp(Hyp0, Hyp) :-
  choose_clause(Hyp0, CLause / Vars0, Clauses1, Clauses2),
  conc(CLauses1, [Clause / Vars | Clauses2], Hyp),
  refine(Clause0, Vars0, CLause, Vars),
  non_redundant(Clause),
  not(unsatisfiable(Clause, Hyp)).

choose_clause(Hyp, CLause, Clauses1, Clauses2) :-
  conc(Clauses1, [ Clause | Clauses2], Hyp),
  nex(E),
  prove(E, [Clause], yes), ! ; conc(Clauses1, [Clause | Clauses2], Hyp).

refine(Clause, Args, Clause, NewArgs) :-
  conc(Args, [A | Args2], Args),
  member(A, Args2),
  conc(Args1, Args2, NewArgs).

refine(Clause, Args0, Clause, Args) :-
  del(Var:Type, Args0, Args1),
  term(Type, Var, Vars),
  conc(Args1, Vars, Args).

refine(Clause, Args, NewClause, NewArgs) :-
  length(Clause, L),
  max_clause_length(MaxL),
  L < MaxL,
  backliteral(Lit, InArgs, RestArgs),
  conc(Clause, [Lit], NewClause),
  connect_inputs(Args, InArgs),
  conc(Args, RestArgs, NewArgs).

non_redundant([_]).
non_redundant([Lit1 | Lits]) :-
  not(literal_member(Lit1, Lits),
  non_redundant(Lits).

literal_member(X, [X1 | Xs]) :-
  X == X1, ! ; literal_member(X, Xs).

show_hyp([]) :- nl.
show_hyp([C / Vars | Cs]) :-
  nl, copy_term( C / Vars, C1 / Vars1),
  name_vars(Vars1, ['A','B','C','D','E','F','G','H','I','J','K','L','M','N']),
  show_clause(C1),
  show_hyp(Cs), !.

show_clause([ X | Xs]) :-
  write(X),
  (Xs = [] ; write(':-'), nl
  write_body(Xs).

write_body([]) :-
  write('.'), !.

write_body([ X | Xs]) :-
  !, tab(2), write(X),
  (Xs = [], !, write('.'), nl ; write(','), nl, write_body(Xs)).

name_vars([], _).
name_vars([Name:Type | Xs], [Name | Names]) :-
  name_vars(Xs, Names).

connect_inputs(_, []).
connect_inputs(S, [X | Xs]) :-
  member(X, S),
  connect_inputs(S, Xs).

merge([], L, L) :- !.
merge(L, [], L) :- !.
merge([X1 | L1], [X2 | L2], [X1 | L3]) :-
  X1 @=< X2, !, merge(L1, [X2 | L2], L3).
merge(L1, [X2 | L2], [X2 | L3]) :-
  merge(L1, L2, L3).

mergesort([],[]) :- !.
mergesort([X],[X]) :- !.
mergesort(L, S) :-
  split(L, L1, L2),
  mergesort(L1, S1),
  mergesort(L2, L2),
  merge(S1, S2, S).

split([],[],[]).
split([X], [X], []).
split([X1, X2 | L], [ X1 | L], [X2 | L2]) :-
  split(L, L1, L2).

init_counts :-
  retract(counter(_,_)), fail ; assert( counter(generated, 0)),
                                assert( counter(complete, 0)),
                                assert( counter(refined, 0)).

add1(C) :-
  retract(counter(C, N)), !, N1 is N+1, assert( counter(C, N1)).

show_counts :-
  counter(generated, NG), counter(refined, NR), counter(complete, NC),
  nl, write('Hypotheses generated: '), write(NG),
  nl, write('Hypotheses refined: '), write(NR),
  TBR is NC - NR,
  nl, write('To be refined: '), write(TBR), nl.

max_proof_length(6).
max_clauses(4).
max_clause_length(5).
