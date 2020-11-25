prefix(PREF,LIST):-append(PREF,_,LIST).
sublist(SUB,LIST):-prefix(SUB,LIST).
sublist(SUB,[_|REST]):-sublist(SUB,REST).

displayGrid(_,0).
displayGrid(G,N):- N > 0, N1 is N-1,
                    write('|'), displayRow(G, N), displayGrid(G, N1).
myNth1(N, C, X):- length(C, L), L >= N -> nth1(N,C,X) ; X=' '.

displayRow([], _):- write('\n').
displayRow([C|G],N):- myNth1(N, C, X), write(X), write('|'), displayRow(G, N).

% replace(L,P,E,R), where L is the current list, P is the position where the element will be insert, E is the element to insert and R is the return, the new list.
replaceInThePosition([_|T],1,E,[E|T]).
replaceInThePosition([H|T],P,E,[H|R]) :-
    P > 1, NP is P-1, replaceInThePosition(T,NP,E,R).

addAtEnd(JOUEUR, COLONNE, NEWCOLONNE) :- append(COLONNE,[JOUEUR],NEWCOLONNE).

saveMove(GRILLE, INDEX, JOUEUR, _) :- INDEX <1 ; INDEX > 7 ; nth1(INDEX, GRILLE, COLONNE), length(COLONNE,L), L>5, play(GRILLE, JOUEUR).
saveMove(GRILLE, INDEX, JOUEUR, NEWGRILLE) :- INDEX >0, INDEX =< 7, nth1(INDEX, GRILLE, COLONNE), length(COLONNE,L), L<6,
		addAtEnd(JOUEUR, COLONNE, NEWCOLONNE), replaceInThePosition(GRILLE, INDEX, NEWCOLONNE, NEWGRILLE).
		
		
		
isWinningColumn([], _) :- false.
isWinningColumn([COLUMN|_], JOUEUR) :- sublist([JOUEUR,JOUEUR,JOUEUR,JOUEUR],COLUMN).
isWinningColumn([_|RESTGRILLE], JOUEUR) :- isWinningColumn(RESTGRILLE, JOUEUR). 
					 
isWinningLine(GRILLE, JOUEUR ) :- isWinningLine(GRILLE, JOUEUR, 1).
isWinningLine(GRILLE, JOUEUR, INDEX) :-  maplist(elementOrEmpty(INDEX),GRILLE,LINE), sublist([JOUEUR,JOUEUR,JOUEUR,JOUEUR], LINE).
isWinningLine(GRILLE, JOUEUR, INDEX) :-  INDEX <6, N is INDEX+1, isWinningLine(GRILLE, JOUEUR, N).

elementOrEmpty(N, L, []):- length(L, LONG), LONG < N.
elementOrEmpty(N,L,X):- nth1(N,L,X).

isWinningDiag1(_,DIAG,JOUEUR,0):- sublist([JOUEUR,JOUEUR,JOUEUR,JOUEUR],DIAG).

isWinningDiag1(GRILLE,DIAG,JOUEUR,N):- N > 0,
					  maplist(elementOrEmpty(N), GRILLE, L),
					  elementOrEmpty(N,L,E),
					  N1 is N-1,
					  isWinningDiag1(GRILLE,[E|DIAG],JOUEUR,N1).

isWinningDiag1(GRILLE,JOUEUR):- isWinningDiag1(GRILLE,[],JOUEUR,6).

isWinningDiag2(_,DIAG,JOUEUR,0):- sublist([JOUEUR,JOUEUR,JOUEUR,JOUEUR],DIAG).
isWinningDiag2(GRILLE,DIAG,JOUEUR,N):- N > 0,
					  maplist(elementOrEmpty(N), GRILLE, L),
					  N2 is 7-N,
					  elementOrEmpty(N2,L,E),
					  N1 is N-1,
					  isWinningDiag2(GRILLE,[E|DIAG],JOUEUR,N1).

isWinningDiag2(GRILLE,JOUEUR):- isWinningDiag2(GRILLE,[],JOUEUR,6).

isWinningDiag(_,_,X,JOUEUR):- isWinningDiag1(X,JOUEUR),!.
isWinningDiag(_,_,X,JOUEUR):- isWinningDiag2(X,JOUEUR),!.

isWinningDiag(GRILLE,N,X,JOUEUR):- N > 0,
					  maplist(elementOrEmpty(N), GRILLE, L),
					  N1 is N-1,
					  isWinningDiag(GRILLE,N1,[L|X],JOUEUR).

isWinningDiag(GRILLE,JOUEUR):- isWinningDiag(GRILLE,6,[],JOUEUR).

winningPosition(GRILLE, JOUEUR) :- isWinningLine(GRILLE, JOUEUR) ; isWinningColumn(GRILLE, JOUEUR) ; isWinningDiag(GRILLE,JOUEUR). 

fullGrid([]).
fullGrid([HEAD|SUBGRILLE]) :- length(HEAD,7), fullGrid(SUBGRILLE).

nextStep(GRILLE, _) :- fullGrid(GRILLE), write('complete grid, it\'s a draw\n').
nextStep(GRILLE, JOUEUR) :- winningPosition(GRILLE, JOUEUR), write('Joueur '), write(JOUEUR), write(' won, game over').
nextStep(GRILLE, 'x') :- playWinningMove(GRILLE,'o').
nextStep(GRILLE, 'o') :- play(GRILLE,'x').

play(GRILLE, JOUEUR) :- write(JOUEUR), write(' player, enter a valid column number N (1<=N<=7) : '), read(N), saveMove(GRILLE, N, JOUEUR, NEWGRILLE), displayGrid(NEWGRILLE,6),write('\n'),nextStep(NEWGRILLE,JOUEUR).

playIARandom(GRILLE, JOUEUR):- random_between(1,7, N), nth1(N, G, X), length(X, L), L < 6 -> (saveMove(GRILLE, N, JOUEUR, NEWGRILLE), displayGrid(NEWGRILLE,6),write('\n'),nextStep(NEWGRILLE,JOUEUR)); playIARandom(GRILLE, JOUEUR).

playWinningMove(GRILLE, JOUEUR):- playWinningMove(GRILLE, JOUEUR, 1).

playWinningMove(GRILLE, JOUEUR, COLONNE):- nth1(COLONNE, G, X), length(X, L), L < 6 -> (saveMove(GRILLE, COLONNE, JOUEUR, NEWGRILLE), winningPosition(NEWGRILLE, JOUEUR), displayGrid(NEWGRILLE,6),write('\n'),nextStep(NEWGRILLE,JOUEUR)) ; (N is COLONNE + 1, playWinningMove(GRILLE, JOUEUR, N)).
playWinningMove(GRILLE, JOUEUR, COLONNE):- nth1(COLONNE, G, X), length(X, L), L < 6 -> (saveMove(GRILLE, COLONNE, JOUEUR, NEWGRILLE), not(winningPosition(NEWGRILLE, JOUEUR)), N is COLONNE + 1, playWinningMove(GRILLE, JOUEUR, N)) ; (N is COLONNE + 1, playWinningMove(GRILLE, JOUEUR, N)).
playWinningMove(GRILLE, JOUEUR, 8):- playIARandom(GRILLE, JOUEUR).


start :- nextStep([[],[],[],[],[],[],[]],'x').

