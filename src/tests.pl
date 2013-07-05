parent(ann,bob).
parent(bob,cat).
parent(tom,bob).
mar(X,Y) :- parent(X,Z),parent(Y,Z).


% -- factorial

fac(N) :- testloop(N, 1).
testloop(0, P) :- write(P).
testloop(N, P) :- N>0, M is N-1, K is N * P, testloop(M, K).

% -- lists

conc([], L, L).
conc([X|L1], L2, [X|L3]) :- conc(L1, L2, L3).

head(X, L) :-
  conc([X], _, L).

% -- arithmetic ops

max(A, B, Result) :-
  ( A > B -> Result is A; Result is B ),
  write(Result).

% ---- FLIGHT PLANNER ----

:- op(50, xfy, :).

route(P1, P2, Day, [ P1 / P2 / Fnum / Deptime ]) :-
  flight(P1, P2, Day, Fnum, Deptime, _ ). % direct flight

route(P1, P2, Day, [ (P1/P3/Fnum1/Dep1) | RestRoute]) :-
  route(P3, P2, Day, RestRoute),
  flight(P1, P3, Day, Fnum1, Dep1, Arr1),
  deptime(RestRoute, Dep2),
  transfer(Arr1, Dep2).

flight(Place1, Place2, Day, Fnum, Deptime, Arrtime) :-
  timetable(Place1, Place2, Flightlist),
  member(Deptime / Arrtime / Fnum / Daylist, Flightlist),
  flyday(Day, Daylist).

flyday(Day, Daylist) :-
  member(Day, Daylist).

flyday(Day, everyday) :-
  member(Day, [mo, tu, we, th, fr, sa, su]).

deptime([ P1 / P2 / Fnum / Dep | _ ], Dep).

transfer(Hours1:Mins1, Hours2:Mins2) :-        % transfer is possible if theere
  60*(Hours2 - Hours1) + Mins2 - Mins1 >= 60.  % is at least 60m difference.

member(X, [ X | L ] ).

member(X, [ Y | L ] ) :-
  member(X, L).

%------- 

not(A) :-
  A, !, fail
  ; true.


