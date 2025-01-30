% Initialise un plateau vide
init_board(Board) :-
    Board = [[e, e, e, e, e, e, e],
             [e, e, e, e, e, e, e],
             [e, e, e, e, e, e, e],
             [e, e, e, e, e, e, e],
             [e, e, e, e, e, e, e],
             [e, e, e, e, e, e, e]].

% Affiche le plateau
display_board(Board) :-
    nl, write('  1   2   3   4   5   6   7'), nl,
    maplist(display_row, Board), nl.


display_row(Row) :-
    write('|'),
    maplist(display_cell, Row),
    nl.

display_cell(Cell) :-
    (Cell = e -> write('   '); write(' '), write(Cell), write(' ')),
    write('|').

% Insère un jeton dans une colonne
insert_in_column(Board, Col, Mark, NewBoard) :-
    transpose(Board, Transposed), % Transposer pour accéder aux colonnes
    nth1(Col, Transposed, Column), % Récupérer la colonne spécifiée
    reverse(Column, ReversedColumn), % Commencer par le bas
    replace_first_empty(ReversedColumn, Mark, UpdatedReversedColumn),
    reverse(UpdatedReversedColumn, UpdatedColumn), % Restaurer l'ordre
    replace_column(Transposed, Col, UpdatedColumn, UpdatedTransposed), % Mettre à jour la colonne
    transpose(UpdatedTransposed, NewBoard). % Re-transposer pour restaurer le format original

replace_first_empty([e|Rest], Mark, [Mark|Rest]).
replace_first_empty([Cell|Rest], Mark, [Cell|UpdatedRest]) :-
    replace_first_empty(Rest, Mark, UpdatedRest).

replace_column([Col|Cols], 1, NewCol, [NewCol|Cols]).
replace_column([Col|Cols], Index, NewCol, [Col|UpdatedCols]) :-
    Index > 1,
    NextIndex is Index - 1,
    replace_column(Cols, NextIndex, NewCol, UpdatedCols).

% Vérifie une victoire
win(Board, Mark) :-
    (   horizontal_win(Board, Mark)
    ;   vertical_win(Board, Mark)
    ;   diagonal_win(Board, Mark)
    ).

horizontal_win(Board, Mark) :-
    member(Row, Board),
    consecutive_four(Row, Mark).

vertical_win(Board, Mark) :-
    transpose(Board, Transposed),
    member(Column, Transposed),
    consecutive_four(Column, Mark).

diagonal_win(Board, Mark) :-
    diagonals_desc(Board, DescDiagonals),
    diagonals_asc(Board, AscDiagonals),
    append(DescDiagonals, AscDiagonals, AllDiagonals),
    member(Diagonal, AllDiagonals),
    consecutive_four(Diagonal, Mark).

diagonals_desc(Board, Diagonals) :-
    findall(Diagonal, diagonal_desc(Board, Diagonal), Diagonals).

diagonal_desc(Board, Diagonal) :-
    length(Board, NumRows),
    length(Board, NumCols),
    (   between(1, NumRows, StartRow), StartCol = 1  % Diagonales partant de la première colonne
    ;   StartRow = 1, between(2, NumCols, StartCol)  % Diagonales partant de la première ligne
    ),
    extract_diagonal_desc(Board, StartRow, StartCol, Diagonal).

extract_diagonal_desc(_, Row, Col, []) :-
    Row > 6 ; Col > 7, !.  % Hors limites
extract_diagonal_desc(Board, Row, Col, [Elem|Diagonal]) :-
    nth1(Row, Board, Line),
    nth1(Col, Line, Elem),
    NextRow is Row + 1,
    NextCol is Col + 1,
    extract_diagonal_desc(Board, NextRow, NextCol, Diagonal).

diagonals_asc(Board, Diagonals) :-
    findall(Diagonal, diagonal_asc(Board, Diagonal), Diagonals).

diagonal_asc(Board, Diagonal) :-
    length(Board, NumRows),
    length(Board, NumCols),
    (   between(1, NumRows, StartRow), StartCol = 1  % Diagonales partant de la première colonne
    ;   StartRow = NumRows, between(2, NumCols, StartCol)  % Diagonales partant de la dernière ligne
    ),
    extract_diagonal_asc(Board, StartRow, StartCol, Diagonal).

extract_diagonal_asc(_, Row, Col, []) :-
    Row < 1 ; Col > 7, !.  % Hors limites
extract_diagonal_asc(Board, Row, Col, [Elem|Diagonal]) :-
    nth1(Row, Board, Line),
    nth1(Col, Line, Elem),
    NextRow is Row - 1,
    NextCol is Col + 1,
    extract_diagonal_asc(Board, NextRow, NextCol, Diagonal).


consecutive_four(List, Mark) :-
    append(_, [Mark, Mark, Mark, Mark|_], List).

% Vérifie si une colonne est valide
valid_column(Board, Col) :-
    nonvar(Board),                         % Vérifie que le plateau est instancié
    between(1, 6, Col),                    % Génère les colonnes valides (1 à 7)
    nth1(Col, Board, Column),              % Récupère la colonne correspondante
    member(e, Column).                     % Vérifie s'il y a une case vide dans la colonne

% Boucle principale
play :-
    write('Choose AI difficulty (1: Easy, 2: Medium, 3: Hard): '),
    read(Difficulty),
    init_board(Board),
    display_board(Board),
    play_turn(Board, x, Difficulty). % Transmet le niveau de difficulté

% Tour de jeu
play_turn(Board, Player, Difficulty) :-
    (   Player = x
    ->  format('Player ~w, choose a column (1-7): ', [Player]),
        read(Col),
        (   valid_column(Board, Col)
        ->  insert_in_column(Board, Col, Player, NewBoard), % Met à jour le plateau
            display_board(NewBoard),
            (   win(NewBoard, Player)
            ->  format('Player ~w wins!~n', [Player])
            ;   next_player(Player, NextPlayer),
                play_turn(NewBoard, NextPlayer, Difficulty) % Passe le nouveau plateau
            )
        ;   write('Invalid move. Try again.'), nl,
            play_turn(Board, Player, Difficulty) % Rejoue le tour sans changer le plateau
        )
    ;   ai_move(Board, Player, NewBoard, Difficulty), % L'IA joue son tour
        display_board(NewBoard),
        (   win(NewBoard, Player)
        ->  write('AI wins!~n')
        ;   next_player(Player, NextPlayer),
            play_turn(NewBoard, NextPlayer, Difficulty) % Passe le plateau mis à jour
        )
    ).


% IA : Détermine le mouvement en fonction du niveau
ai_move(Board, Player, NewBoard, 1) :- % Niveau 1 : Aléatoire
    random_ai_move(Board, Player, NewBoard).
ai_move(Board, Player, NewBoard, 2) :- % Niveau 2 : À implémenter
    write('Medium AI is not implemented yet. Playing randomly.'), nl,
    random_ai_move(Board, Player, NewBoard).
ai_move(Board, Player, NewBoard, 3) :- % Niveau 3 : À implémenter
    write('Hard AI is not implemented yet. Playing randomly.'), nl,
    random_ai_move(Board, Player, NewBoard).

% IA aléatoire : Joue dans une colonne valide choisie au hasard
random_ai_move(Board, Player, NewBoard) :-
    nonvar(Board),                                % Vérifie que `Board` est instancié
    findall(Col, valid_column(Board, Col), ValidMoves), % Liste des colonnes valides
    ValidMoves \= [],                             % Vérifie qu'il existe des colonnes valides
    random_member(Col, ValidMoves),              % Choisit une colonne au hasard
    insert_in_column(Board, Col, Player, NewBoard).


% Inverse le joueur
next_player(x, o).
next_player(o, x).

% Transpose une matrice
transpose([[]|_], []) :- !.
transpose(Matrix, [Col|Cols]) :-
    maplist(head_tail, Matrix, Col, RestMatrix),
    transpose(RestMatrix, Cols).

head_tail([H|T], H, T).
