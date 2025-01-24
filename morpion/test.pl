% Définition du plateau (board)
:- dynamic board/1.

% Initialisation du plateau
initialize_board :-
    retractall(board(_)),        
    blank_mark(E),
    asserta(board([[E, E, E, E, E, E, E],
                   [E, E, E, E, E, E, E],
                   [E, E, E, E, E, E, E],
                   [E, E, E, E, E, E, E],
                   [E, E, E, E, E, E, E],
                   [E, E, E, E, E, E, E]])).

% Définition du symbole pour une case vide
blank_mark(e).

% Vérification si le plateau est entièrement vide
is_board_empty :-
    board(Board), % Récupération du plateau dynamique
    blank_mark(E),
    forall(member(Row, Board), forall(member(Cell, Row), Cell = E)).

% Affichage du plateau (ligne par ligne)
print_board :-
    board(Board), % Récupération du plateau dynamique
    print_rows(Board).

print_rows([]) :- !. % Fin du plateau
print_rows([Row|Rest]) :-
    print_row(Row),  % Affiche une ligne
    nl,              % Retour à la ligne
    print_rows(Rest).

% Affichage d'une ligne
print_row([]) :- !. % Fin de la ligne
print_row([Cell|Rest]) :-
    write(Cell), write(' '),  % Affiche une cellule et ajoute un espace
    print_row(Rest).
