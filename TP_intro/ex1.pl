grand_parent(X,Y):- parent(X,Z), parent(Z,Y).
ancetre(X,Y):- parent(X,Y).
ancetre(X,Y):- parent(X,Z), ancetre(Z,Y).
afficher_petits_enfants(X):-forall(grand_parent(X,Y), (write(Y), write('\n'))).
liste_des_ancetres(L, X):- forall(ancetre(Z,X), member(Z, L)), forall(member(Z,L), ancetre(Z,X)).
frere(X,Y):- parent(Z,X), parent(Z,Y), X\==Y, masculin(X).
cousin(X,Y):- grand_parent(Z,X), grand_parent(Z,Y), X\==Y, masculin(X).
oncle_ou_tante(X,Y):- not(parent(X,Y)), parent(Z,X), grand_parent(Z, Y).



parent(florian, julien).
parent(julien, pierre).
parent(julien, elie).
parent(pierre, adele).
parent(pierre, elod).
parent(elie, eric).
parent(elie, donald).
masculin(pierre).
masculin(julien).
masculin(florian).
masculin(elie).
masculin(elod).
masculin(eric).
masculin(donald).
feminin(adele).

