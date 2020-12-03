grilleValeur([[0,  0,   0,  0,  0,  0,  0,  0,  0, 0],
              [0,  4,  -3,  2,  2,  2,  2, -3,  4, 0],
              [0, -3,  -4, -1, -1, -1, -1, -4, -3, 0],
              [0,  2,  -1,  1,  0,  0,  1, -1,  2, 0],
              [0,  2,  -1,  0,  1,  1,  0, -1,  2, 0],
              [0,  2,  -1,  0,  1,  1,  0, -1,  2, 0],
              [0,  2,  -1,  1,  0,  0,  1, -1,  2, 0],
              [0, -3,  -4, -1, -1, -1, -1, -4, -3, 0],
              [0,  4,  -3,  2,  2,  2,  2, -3,  4, 0],
              [0,  0,   0,  0,  0,  0,  0,  0,  0, 0]]).

incrScore(GrilleValeur,Ligne,Colonne,Score):-
    element(GrilleValeur,Ligne,Colonne,Element),
    write(Element),nl,
    Score is Score + Element.

calculerScore(Grille,Joueur,GrilleValeur,Score):-
    coupExistant(Ligne,Colonne),
    element(Grille,Ligne,Colonne,Joueur),
    incrScore(GrilleValeur,Ligne,Colonne,Score).



grilleTemporaire(Grille,GrilleTemp):-
    GrilleTemp=Grille.




estUnMeilleurMove(BestScore,BestScoreAutreCoup,AutreLigne,AutreColonne,BestLigne,BestColonne):-
   (BestScore<BestScoreAutreCoup,
    BestLigne=AutreLigne,
    BestColonne=AutreColonne,
   BestScore=BestScoreAutreCoup).

estUnPireMove(BestScore,BestScoreAutreCoup,AutreLigne,AutreColonne,BestLigne,BestColonne):-
   (BestScore>BestScoreAutreCoup,
    BestLigne=AutreLigne,
    BestColonne=AutreColonne,
   BestScore=BestScoreAutreCoup).

minimax(Longueur,_,_,Grille,_,_,_,_):-
   Longueur==0,
   partieFinie(Grille).


minimax(Longueur,Joueur,JoueurActuel,Grille,BestLigne,BestColonne,BestScore,Score):-
    grilleValeur(GrilleValeur),
    not(inv(Joueur,JoueurActuel)),
    coupExistant(Ligne,Colonne),
    coupValide(Grille,Ligne,Colonne,JoueurActuel),
    grilleTemporaire(Grille,GrilleTemp),
    %afficherGrille(GrilleTemp,JoueurActuel),
    write("profondeur : "),write(Longueur),write("  "),write(Ligne),write(" , "),write(Colonne),nl,
    jouerMouvement(GrilleTemp,Ligne,Colonne,Joueur,GrilleFinale),
    inv(JoueurActuel,JoueurSuivant),
    decr(Longueur,LongueurN),
    minimax(LongueurN,Joueur,JoueurSuivant,GrilleFinale,BestLigne,BestColonne,BestScore,ScoreInf),
    calculerScore(Grille,Joueur,GrilleValeur,Score),
    estUnMeilleurMove(BestScore,ScoreInf,Ligne,Colonne,BestLigne,BestColonne).

minimax(Longueur,Joueur,JoueurActuel,Grille,BestLigne,BestColonne,BestScore,Score):-
    grilleValeur(GrilleValeur),
    inv(Joueur,JoueurActuel),
    coupExistant(Ligne,Colonne),
    coupValide(Grille,Ligne,Colonne,JoueurActuel),
    grilleTemporaire(Grille,GrilleTemp),
     write("profondeur : "),write(Longueur),write("  "),write(Ligne),write(" , "),write(Colonne),nl,
    jouerMouvement(GrilleTemp,Ligne,Colonne,Joueur,GrilleFinale),
    inv(JoueurActuel,JoueurSuivant),
     decr(Longueur,LongueurN),
    minimax(LongueurN,Joueur,JoueurSuivant,GrilleFinale,BestLigne,BestColonne,BestScore,ScoreInf),
    calculerScore(Grille,Joueur,GrilleValeur,Score),
    estUnPireMove(BestScore,ScoreInf,Ligne,Colonne,BestLigne,BestColonne).


