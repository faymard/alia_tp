membre(X,[X|_]).
membre(X,[_|L]):- membre(X,L).

membre_unique(X, [X|L2]):- not(member(X, L2)).
membre_unique(X, [_|L1]):- membre_unique(X, L1).

%True when L3 is concatenation of L1 and L2
concat([], [], []).
concat([X], [Y], [X,Y]).
concat(L1, [], L1).
concat([], L2, L2).
concat(L1, L2, L3):- append(L1,L2,L4), list_equal(L4, L3).

list_equal([], []).
list_equal([X|L1], [Y|L2]):- X == Y, list_equal(L1, L2).

dernier(X, [X]).
dernier(X, [Y|L]):- X\==Y, dernier(X,L).

element(_, [], []).
element(X,[Y|L2],R):- X == Y, element(X, L2, R).
element(X,[Y|L2],[Z|R2]):- X \== Y, Y == Z,element(X, L2, R2).


composante(I, X, L):- comp(I,1,X,L).
comp(I, C, X, [Y|_]):- C == I, X == Y.
comp(I, C, X, [_|L1]):- C \== I, C1 is C+1, comp(I, C1, X, L1).


subsAll(_, _,[], []).
subsAll(X, Y,[Z|L2],[T|R2]):- X == Z, T == Y,subsAll(X, Y, L2, R2).
subsAll(X,Y,[Z|L2],[T|R2]):- X \== Z,Z == T,subsAll(X, Y,L2, R2).



list2ens(L1, [Y|L2]):- forall(member(X,L1), member(X, [Y|L2])), uniq(L1, [Y|L2]).
uniq(_, []).
uniq(L1, [Y|L2]):- member(Y, L1), not(member(Y, L2)), uniq(L1, L2).

