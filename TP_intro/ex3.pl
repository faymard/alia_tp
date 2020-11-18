sorted([],[]).
sorted(L1,[]) :- false.
sorted([],L2).
sorted([X|L1],[Y|L2]) :- X=<Y, sorted(L1,L2).
avant(A,B) :- name(A,N1),name(B,N2),sorted(N1,N2).