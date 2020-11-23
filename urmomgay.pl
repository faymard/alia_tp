displayGrid(G,N):- N > 0, N1 is N-1,
                    write('|'), displayRow(G, N), displayGrid(G, N1).
displayGrid(G,0).
myNth1(N, C, X):- length(C, L), L >= N -> nth1(N,C,X) ; X=' '.

displayRow([], N):- write('\n').
displayRow([C|G],N):- myNth1(N, C, X), write(X), write('|'), displayRow(G, N).

% replace(L,P,E,R), where L is the current list, P is the position where the element will be insert, E is the element to insert and R is the return, the new list.
replaceInThePosition([_|T],1,E,[E|T]).
replaceInThePosition([H|T],P,E,[H|R]) :-
    P > 1, NP is P-1, replaceInThePosition(T,NP,E,R).

jouer:- jouerCoupO([[],[],[],[],[],[],[]]).
addAtEnd(JOUEUR, COLONNE, NEWCOLONNE) :- append(COLONNE,[JOUEUR],NEWCOLONNE).

saveMove(GRILLE, INDEX, JOUEUR) :- INDEX >0, INDEX =< 7, nth1(INDEX, GRILLE, COLONNE), length(COLONNE,L), L<6, write('old column : '),write(COLONNE), 
		addAtEnd(JOUEUR, COLONNE, NEWCOLONNE), write(' ,new column : '),write(NEWCOLONNE),write('\n'), replaceInThePosition(GRILLE, INDEX, NEWCOLONNE, NEWGRILLE), write('nouvelle grille : '), write(NEWGRILLE).
		
%winningPosition(GRILLE, JOUEUR) :- 

isWinningColumn([], JOUEUR) :- false.
isWinningColumn([COLUMN|RESTGRILLE], JOUEUR) :- write('column : '), write(COLUMN), sublist([JOUEUR,JOUEUR,JOUEUR,JOUEUR],COLUMN).
isWinningColumn([COLUMN|RESTGRILLE], JOUEUR) :- isWinningColumn(RESTGRILLE, JOUEUR). 
					 
isWinningLine(GRILLE, JOUEUR ) :- isWinningLine(GRILLE, JOUEUR, 1).
isWinningLine(GRILLE, JOUEUR, INDEX) :-  maplist(nth1(INDEX),GRILLE,LINE), sublist([JOUEUR,JOUEUR,JOUEUR,JOUEUR], LINE).
isWinningLine(GRILLE, JOUEUR, INDEX) :-  INDEX <6, N is INDEX+1, isWinningLine(GRILLE, JOUEUR, N).


isWinningDiag1(GRILLE,DIAG,JOUEUR,1):- write(DIAG),
sublist([JOUEUR,JOUEUR,JOUEUR,JOUEUR],DIAG).

isWinningDiag1(GRILLE,DIAG,JOUEUR,N):- write(DIAG), N > 1,
					  maplist(nth1(N), GRILLE, L),
					  write('ur mom gay'),
					  write(L),
					  nth1(N,L,E),
					  N1 is N-1,
					  isWinningDiag1(GRILLE,[E|DIAG],JOUEUR,N1).

isWinningDiag1(GRILLE,JOUEUR):- isWinningDiag1(GRILLE,[],JOUEUR,7).

isWinningDiag2(GRILLE,DIAG,JOUEUR,1):- write(DIAG),sublist([JOUEUR,JOUEUR,JOUEUR,JOUEUR],DIAG).
isWinningDiag2(GRILLE,DIAG,JOUEUR,N):- N > 1,
					  maplist(nth1(N), GRILLE, L),
					  N2 is 8-N,
					  nth1(N2,L,E),
					  N1 is N-1,
					  isWinningDiag2(GRILLE,[E|DIAG],JOUEUR,N1).

isWinningDiag2(GRILLE,JOUEUR):- isWinningDiag2(GRILLE,[],JOUEUR,7).

% nextStep
% winningPosition
% fullGrid
% nextPlayer

