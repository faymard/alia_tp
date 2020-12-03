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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/* Diagonal End Game Test
  J : Joueur
  G : Grille de jeu
*/
diag1([L|Q],J,I,N):- LI is I+1, nth0(LI,L,J), N is 1.
diag1([L|Q],J,I,N):- LI is I+1, nth0(LI,L,J), diag1(Q,J,LI,R), N is R+1.
diagonalEndGame([L|Q],J,N):- nth0(I,L,J), diag1(Q,J,I,R), N is R+1.
diagonalEndGame(L,J):- diagonalEndGame(L,J,N), N == 4.
diagonalEndGame([_|G],J):- diagonalEndGame(G,J).

diag1Bis([L|Q],J,I,N):- LI is I-1, nth0(LI,L,J), N is 1.
diag1Bis([L|Q],J,I,N):- LI is I-1, nth0(LI,L,J), diag1Bis(Q,J,LI,R), N is R+1.
diagonalEndGameBis([L|Q],J,N):- nth0(I,L,J), diag1Bis(Q,J,I,R), N is R+1.
diagonalEndGameBis(L,J):- diagonalEndGameBis(L,J,N), N == 4.
diagonalEndGameBis([_|G],J):- diagonalEndGameBis(G,J).

/* Test à lancer pour le test de fin diagonale*/
diagonalEnd(G,J):- diagonalEndGame(G,J); diagonalEndGameBis(G,J).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

winningPosition(GRILLE, JOUEUR) :- isWinningLine(GRILLE, JOUEUR) ; isWinningColumn(GRILLE, JOUEUR) ; diagonalEnd(GRILLE,JOUEUR).

fullGrid([]).
fullGrid([HEAD|SUBGRILLE]) :- length(HEAD,7), fullGrid(SUBGRILLE).



play(GRILLE, JOUEUR) :- write(JOUEUR), write(' player, enter a valid column number N (1<=N<=7) : '), read(N), saveMove(GRILLE, N, JOUEUR, NEWGRILLE), displayGrid(NEWGRILLE,6),write('\n'),nextStep(NEWGRILLE,JOUEUR).

playIARandom(GRILLE, JOUEUR):- random_between(1,7, N), nth1(N, GRILLE, X), length(X, L), L < 6 -> (saveMove(GRILLE, N, JOUEUR, NEWGRILLE), displayGrid(NEWGRILLE,6),write('\n'),nextStep(NEWGRILLE,JOUEUR)); playIARandom(GRILLE, JOUEUR).

possibleMove(JOUEUR,GRILLE, NEWGRILLE) :- INDEX >0, INDEX =< 7, nth1(INDEX, GRILLE, COLONNE), length(COLONNE,L), L<6,
		addAtEnd(JOUEUR, COLONNE, NEWCOLONNE), replaceInThePosition(GRILLE, INDEX, NEWCOLONNE, NEWGRILLE).

allMoves([],J,I,LC).
allMoves([L|Q],J,I,LC):- (length(L,T), T < 6, append(L3,[I],LC), NEWI is I+1, allMoves(Q,J,NEWI,L3)); (length(L,T), T >= 6, NEWI is I+1, allMoves(Q,J,NEWI,LC)).
allMoves(G,J,ListeCoups):- I is 1, allMoves(G,J,I,ListeCoups).

allPossibleMoves(JOUEUR, GRILLE, AllMoves) :- allMoves(GRILLE,JOUEUR,ListeCoups), 
				 allPossibleMoves(JOUEUR, GRILLE, AllMoves, ListeCoups,1).


allPossibleMoves(JOUEUR, GRILLE, AllMoves, ListeCoups,INDEX):- length(ListeCoups, L), INDEX =< L, nth1(INDEX, ListeCoups, C),
	 I is INDEX + 1,allPossibleMoves(JOUEUR, GRILLE, GRILLESUIVANTE, ListeCoups,I), 
	 saveMove(GRILLE, C, JOUEUR, NEWGRILLE), append(GRILLESUIVANTE, [NEWGRILLE], AllMoves).

allPossibleMoves(JOUEUR, GRILLE, GRILLESUIVANTE, ListeCoups,INDEX):- length(ListeCoups, L), INDEX > L, GRILLESUIVANTE = [].

evalBoards([], Value) :-
    Value is 0.
evalBoards(Board, Value) :-
    winningPosition(Board, 'o'),
    Value is 500000.
evalBoards(Board, Value) :-
    winningPosition(Board, 'x'),
    Value is -500000.
evalBoards(Board, Value) :-
    fullGrid(Board),
    Value is 0.
evalBoards(Board, Value) :-
    score(Board, Value).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Score pour minmax
score(GRILLE, SCORE):- score(GRILLE,  1,1, SCORE, [[3,4,5,5,4,3], [4,6,8,8,6,4],[5,8,11,11,8,5], [7,10,13,13,10,7], [5,8,11,11,8,5], [4,6,8,8,6,4],[3,4,5,5,4,3]]).
score(GRILLE, LIGNE, COLONNE, SCORE, HEURISTIQUE):- COLONNE < 8, LIGNE == 7, NC is COLONNE + 1,
													score(GRILLE, 1, NC, SCORE, HEURISTIQUE).

score(GRILLE, LIGNE, COLONNE, SCORE, HEURISTIQUE):- COLONNE == 8, SCORE is 0.
score(GRILLE, LIGNE, COLONNE, SCORE, HEURISTIQUE):- COLONNE < 8, LIGNE < 7, nth1(COLONNE, GRILLE, C), elementOrEmpty(LIGNE, C, X), 
													X \= [] -> ( 
													X == 'o' -> (nth1(COLONNE, HEURISTIQUE, CH), nth1(LIGNE, CH, LH), NL is LIGNE + 1, score(GRILLE, NL, COLONNE, SCORE_APRES, HEURISTIQUE), SCORE is LH + SCORE_APRES ); 
																nth1(COLONNE, HEURISTIQUE, CH), nth1(LIGNE, CH, LH), NL is LIGNE + 1, score(GRILLE, NL, COLONNE, SCORE_APRES, HEURISTIQUE), SCORE is SCORE_APRES - LH 
													); NL is LIGNE + 1, score(GRILLE, NL, COLONNE, SCORE_APRES, HEURISTIQUE), SCORE is SCORE_APRES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


switchMaxMin(max, min).
switchMaxMin(min, max).


compareMoves(max, MoveA, ValueA, _, ValueB, MoveA, ValueA) :-
	ValueA >= ValueB.
compareMoves(max, _, ValueA, MoveB, ValueB, MoveB, ValueB) :-
	ValueA < ValueB.
compareMoves(min, MoveA, ValueA, _, ValueB, MoveA, ValueA) :-
	ValueA =< ValueB.
compareMoves(min, _, ValueA, MoveB, ValueB, MoveB, ValueB) :-
	ValueA > ValueB.


bestMove(max,[],[],-500000,_).
bestMove(min,[],[],500000,_).

bestMove(MinMax, [Move | RestMoves], BMove, BestValue, 0) :-
    evalBoards(Move, Value),
    bestMove(MinMax, RestMoves, CBMove, CBValue, 0),
	compareMoves(MinMax, Move, Value, CBMove, CBValue, BMove, BestValue).

bestMove(MinMax, [Move | RestMoves], BMove, BestValue, PROFONDEUR) :-
	PROFONDEUR >0,
	bestMove(MinMax, RestMoves, CBMove, CBValue, PROFONDEUR),
	switchMaxMin(MinMax, NextPlayer),
	PROF is PROFONDEUR -1,
	minimaxDepth(NextPlayer, Move, _, BottomBestV, PROF),
	compareMoves(MinMax, Move, BottomBestV, CBMove, CBValue, BMove, BestValue).

playerSymbol(max, 'o').
playerSymbol(min, 'x').

minimaxDepth(MinMax, Board, BMove, BestValue, PROFONDEUR) :-
	playerSymbol(MinMax, Color),
	allPossibleMoves(Color, Board, AllMoves),
    bestMove(MinMax, AllMoves, BMove, BestValue, PROFONDEUR).

minimax(Board, BMove) :- PROFONDEUR is 2,
	minimaxDepth(max, Board, BMove, _, PROFONDEUR).


adversaire('o', 'x').
adversaire('x', 'o').

%%%%%%%%%%%%%%%%%%%%%%%
% Aléatoire sauf si victoire ou défaite au prochain tour (presque équivalent min max profondeur 1)

playWinningMove(GRILLE, JOUEUR):- playWinningMove(GRILLE, JOUEUR, 1).

playWinningMove(GRILLE, JOUEUR, COLONNE):- COLONNE < 8, nth1(COLONNE, GRILLE, X), length(X, L), L < 6, saveMove(GRILLE, COLONNE, JOUEUR, NEWGRILLE), 
										winningPosition(NEWGRILLE, JOUEUR), displayGrid(NEWGRILLE,6),write('\n'),nextStep(NEWGRILLE,JOUEUR), write('found winning position').

playWinningMove(GRILLE, JOUEUR, COLONNE):- COLONNE < 8, nth1(COLONNE, GRILLE, X), length(X, L), L > 5, 
												N is COLONNE + 1, playWinningMove(GRILLE, JOUEUR, N).
playWinningMove(GRILLE, JOUEUR, COLONNE):- COLONNE < 8, nth1(COLONNE, GRILLE, X), length(X, L), L < 6, saveMove(GRILLE, COLONNE, JOUEUR, NEWGRILLE),
													not(winningPosition(NEWGRILLE, JOUEUR)), N is COLONNE + 1, playWinningMove(GRILLE, JOUEUR, N).
													
playWinningMove(GRILLE, JOUEUR, 8):- playDefendingMove(GRILLE, JOUEUR).


playDefendingMove(GRILLE, JOUEUR):- playDefendingMove(GRILLE, JOUEUR, 1).

playDefendingMove(GRILLE, JOUEUR, COLONNE):- COLONNE < 8, adversaire(JOUEUR, ADVERSAIRE),nth1(COLONNE, GRILLE, X), length(X,L), L<6, saveMove(GRILLE, COLONNE, ADVERSAIRE, NEWGRILLE),
										winningPosition(NEWGRILLE, ADVERSAIRE), saveMove(GRILLE, COLONNE, JOUEUR, NEWGRILLE_IA), displayGrid(NEWGRILLE_IA, 6), write('\n'), nextStep(NEWGRILLE_IA, JOUEUR).

playDefendingMove(GRILLE, JOUEUR, COLONNE):-  COLONNE < 8, nth1(COLONNE, GRILLE, X), length(X, L), L > 5,
												N is COLONNE + 1, playDefendingMove(GRILLE, JOUEUR, N).
playDefendingMove(GRILLE, JOUEUR, COLONNE):-  COLONNE < 8, adversaire(JOUEUR, ADVERSAIRE), nth1(COLONNE, GRILLE, X), length(X, L), L < 6, saveMove(GRILLE, COLONNE, ADVERSAIRE, NEWGRILLE),
													not(winningPosition(NEWGRILLE, ADVERSAIRE)), N is COLONNE + 1, playDefendingMove(GRILLE, JOUEUR, N).
													
playDefendingMove(GRILLE, JOUEUR, 8):- JOUEUR == 'x', playIARandom(GRILLE, JOUEUR).
playDefendingMove(GRILLE, JOUEUR, 8):- JOUEUR == 'o', minimax(GRILLE, NEWGRILLE), displayGrid(NEWGRILLE,6),write('\n'),nextStep(NEWGRILLE,'o').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Jouer au centre


middle([X],B):- B is X.
middle([X|Q],B):- middle(Q,R), ((X == 4, B is X); ((X == 3; X == 5), not(R == 4), B is X); ((X == 2; X == 6), not(R == 3), not(R == 4), not(R == 5), B is X); ((X == 1; X == 7), not(R == 3), not(R == 2), not(R == 4), not(R == 6), not(R == 5), B is X); B is R).
middlePlay(G,J,B):- not(fullGrid(G)), allMoves(G,J,ListeCoups), middle(ListeCoups,B).
playIAMiddle(G,J, NEWG):- middlePlay(G,J,B), saveMove(G,B,J,NEWG), displayGrid(NEWG,6).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start :- nextStep([[],[],[],[],[],[],[]],'x').

nextStep(GRILLE, _) :- fullGrid(GRILLE), write('complete grid, it\'s a draw\n').
nextStep(GRILLE, JOUEUR) :- winningPosition(GRILLE, JOUEUR), write('Joueur '), write(JOUEUR), write(' won, game over').
nextStep(GRILLE, 'x') :- playIARandom(GRILLE,'o').
nextStep(GRILLE, 'o') :- playIAMiddle(GRILLE,'x', NEWG), nextStep(NEWG, 'x').