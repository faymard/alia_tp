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
nextStep(GRILLE, 'x') :- minimax(GRILLE, NEWGRILLE), displayGrid(NEWGRILLE,6),write('\n'),nextStep(NEWGRILLE,'o').
nextStep(GRILLE, 'o') :- play(GRILLE,'x').

play(GRILLE, JOUEUR) :- write(JOUEUR), write(' player, enter a valid column number N (1<=N<=7) : '), read(N), saveMove(GRILLE, N, JOUEUR, NEWGRILLE), displayGrid(NEWGRILLE,6),write('\n'),nextStep(NEWGRILLE,JOUEUR).

playIARandom(GRILLE, JOUEUR):- random_between(1,7, N), nth1(N, GRILLE, X), length(X, L), L < 6 -> (saveMove(GRILLE, N, JOUEUR, NEWGRILLE), displayGrid(NEWGRILLE,6),write('\n'),nextStep(NEWGRILLE,JOUEUR)); playIARandom(GRILLE, JOUEUR).

%%%%%%%%%%% MIN MAX %%%%%%%%%%%
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

eval_board([], Value) :-
    Value is 0.
eval_board(Board, Value) :-
    winningPosition(Board, 'o'),
    Value is 500000, !.
eval_board(Board, Value) :-
    winningPosition(Board, 'x'),
    Value is -500000, !.
eval_board(Board, Value) :-
    fullGrid(Board),
    Value is 0.
eval_board(Board, Value) :-
    score(Board, Value).

score(GRILLE, SCORE):- score(GRILLE,  1,1, SCORE, [[3,4,5,5,4,3], [4,6,8,8,6,4],[5,8,11,11,8,5], [7,10,13,13,10,7], [5,8,11,11,8,5], [4,6,8,8,6,4],[3,4,5,5,4,3]]).
score(GRILLE, LIGNE, COLONNE, SCORE, HEURISTIQUE):- COLONNE < 8, LIGNE == 7, NC is COLONNE + 1,
													score(GRILLE, 1, NC, SCORE, HEURISTIQUE).

score(GRILLE, LIGNE, COLONNE, SCORE, HEURISTIQUE):- COLONNE == 8, SCORE is 0.
score(GRILLE, LIGNE, COLONNE, SCORE, HEURISTIQUE):- COLONNE < 8, LIGNE < 7, nth1(COLONNE, GRILLE, C), elementOrEmpty(LIGNE, C, X), 
													X \= [] -> ( 
													X == 'o' -> (nth1(COLONNE, HEURISTIQUE, CH), nth1(LIGNE, CH, LH), NL is LIGNE + 1, score(GRILLE, NL, COLONNE, SCORE_APRES, HEURISTIQUE), SCORE is LH + SCORE_APRES ); %Si c'est une case de l'IA
																nth1(COLONNE, HEURISTIQUE, CH), nth1(LIGNE, CH, LH), NL is LIGNE + 1, score(GRILLE, NL, COLONNE, SCORE_APRES, HEURISTIQUE), SCORE is SCORE_APRES - LH %Si c'est une case de l'adversaire
													); NL is LIGNE + 1, score(GRILLE, NL, COLONNE, SCORE_APRES, HEURISTIQUE), SCORE is SCORE_APRES.


% change_max_min(+MinOrMax, TheOther)
% Changes the MinMax atom.
change_max_min(max, min).
change_max_min(min, max).

% compare_moves(+MinMax, +MoveA, +ValueA, +MoveB, +ValueB, -BetterMove, -BetterValue)
% Chooses the move with the higher value.
compare_moves(max, MoveA, ValueA, _, ValueB, MoveA, ValueA) :-
	ValueA >= ValueB.
compare_moves(max, _, ValueA, MoveB, ValueB, MoveB, ValueB) :-
	ValueA < ValueB.
compare_moves(min, MoveA, ValueA, _, ValueB, MoveA, ValueA) :-
	ValueA =< ValueB.
compare_moves(min, _, ValueA, MoveB, ValueB, MoveB, ValueB) :-
	ValueA > ValueB.

% best_move(+MinMax, +AllMoves, -BestMove, -BestValue)
% Chooses the next move.

best_move(max,[],[],-500,_).
best_move(min,[],[],500,_).

best_move(MinMax, [Move | RestMoves], BestMove, BestValue, 0) :-
    eval_board(Move, Value),
%	write('0'),
    best_move(MinMax, RestMoves, CurrentBestM, CurrentBestV, 0),
	compare_moves(MinMax, Move, Value, CurrentBestM, CurrentBestV, BestMove, BestValue).

best_move(MinMax, [Move | RestMoves], BestMove, BestValue, PROFONDEUR) :-
	PROFONDEUR >0,
%	write(PROFONDEUR),
%	write('\n'),
%	write(RestMoves),
	best_move(MinMax, RestMoves, CurrentBestM, CurrentBestV, PROFONDEUR),
	
	change_max_min(MinMax, Other),
%	write('Other : '),
%	write(Other),
%	write('\n'),
	PROF is PROFONDEUR -1,
	minimaxDepth(Other, Move, _, BottomBestV, PROF),
	compare_moves(MinMax, Move, BottomBestV, CurrentBestM, CurrentBestV, BestMove, BestValue).

% player_color(MinMax, Color)
% Matches the player color based on the MinMax atom.
player_color(max, 'o').
player_color(min, 'x').

% minimaxDepth(+MinMax, +Board, -BestMove, -BestValue)
% Chooses the best possible move for the current board.
minimaxDepth(MinMax, Board, BestMove, BestValue, PROFONDEUR) :-
	player_color(MinMax, Color),
	allPossibleMoves(Color, Board, AllMoves),
    best_move(MinMax, AllMoves, BestMove, BestValue, PROFONDEUR).

% minimax(+Board, -BestMove)
% Matches the next move based on the current board.
minimax(Board, BestMove) :- PROFONDEUR is 2,
	minimaxDepth(max, Board, BestMove, _, PROFONDEUR).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
playWinningMove(GRILLE, JOUEUR):- playWinningMove(GRILLE, JOUEUR, 1).

playWinningMove(GRILLE, JOUEUR, COLONNE):- COLONNE < 8, nth1(COLONNE, GRILLE, X), length(X, L), L < 6, saveMove(GRILLE, COLONNE, JOUEUR, NEWGRILLE), 
										winningPosition(NEWGRILLE, JOUEUR), displayGrid(NEWGRILLE,6),write('\n'),nextStep(NEWGRILLE,JOUEUR), write('found winning position').

playWinningMove(GRILLE, JOUEUR, COLONNE):- COLONNE < 8, nth1(COLONNE, GRILLE, X), length(X, L), L > 5,  write('full column\n'),
												N is COLONNE + 1, playWinningMove(GRILLE, JOUEUR, N).
playWinningMove(GRILLE, JOUEUR, COLONNE):- COLONNE < 8, nth1(COLONNE, GRILLE, X), length(X, L), L < 6, saveMove(GRILLE, COLONNE, JOUEUR, NEWGRILLE),
													not(winningPosition(NEWGRILLE, JOUEUR)), N is COLONNE + 1, playWinningMove(GRILLE, JOUEUR, N).
													
playWinningMove(GRILLE, JOUEUR, 8):- playDefendingMove(GRILLE, JOUEUR).


playDefendingMove(GRILLE, JOUEUR):- write('defending\n'), playDefendingMove(GRILLE, JOUEUR, 1).

playDefendingMove(GRILLE, JOUEUR, COLONNE):- write('defending 1\n'), COLONNE < 8, nth1(COLONNE, GRILLE, X), length(X,L), L<6, saveMove(GRILLE, COLONNE, 'x', NEWGRILLE),
										winningPosition(NEWGRILLE, 'x'), saveMove(GRILLE, COLONNE, JOUEUR, NEWGRILLE_IA), displayGrid(NEWGRILLE_IA, 6), write('\n'), nextStep(NEWGRILLE_IA, JOUEUR).

playDefendingMove(GRILLE, JOUEUR, COLONNE):- write('defending 2\n'), COLONNE < 8, nth1(COLONNE, GRILLE, X), length(X, L), L > 5,  write('full column\n'),
												N is COLONNE + 1, playDefendingMove(GRILLE, JOUEUR, N).
playDefendingMove(GRILLE, JOUEUR, COLONNE):- write('defending 3\n'), COLONNE < 8, nth1(COLONNE, GRILLE, X), length(X, L), L < 6, saveMove(GRILLE, COLONNE, 'x', NEWGRILLE),
													not(winningPosition(NEWGRILLE, 'x')), N is COLONNE + 1, playDefendingMove(GRILLE, JOUEUR, N).
													
%playDefendingMove(GRILLE, JOUEUR, 8):- write('random\n'), playIARandom(GRILLE, JOUEUR).


start :- nextStep([[],[],[],[],[],[],[]],'x').
