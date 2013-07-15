% some background knowledge represented a randomly chosen weighted graph
node(1..4).

edge(1,2;3).
edge(2,3;4).
edge(3,1;4).
edge(4,2;3).

cost(1,2,3).
cost(1,3,1).
cost(2,3,5).
cost(2,4,1).
cost(3,1,2).
cost(3,4,8).
cost(4,2,1).
cost(4,3,9).

1{ loop(A,B) : edge(A,B) } 1 :- node(A).
1{ loop(A,B) : edge(A,B) } 1 :- node(B).
reached(A) :- loop(1,A).
reached(A) :- loop(B,A), reached(B).

:- node(A), not reached(A).

#min [ loop(A,B) : cost(A,B,C) = C ].

% showing loop info
#hide.
#show loop/2.
