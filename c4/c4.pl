% Initialise un plateau vide
init_board([[e, e, e, e, e, e, e],
            [e, e, e, e, e, e, e],
            [e, e, e, e, e, e, e],
            [e, e, e, e, e, e, e],
            [e, e, e, e, e, e, e],
            [e, e, e, e, e, e, e]]).

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
    nth1(Col, Board, Column),
    reverse(Column, ReversedColumn),
    replace_first_empty(ReversedColumn, Mark, UpdatedReversedColumn),
    reverse(UpdatedReversedColumn, UpdatedColumn),
    replace_column(Board, Col, UpdatedColumn, NewBoard).

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
    diagonals(Board, Diagonals),
    member(Diagonal, Diagonals),
    consecutive_four(Diagonal, Mark).

diagonals(Board, Diagonals) :-
    findall(Diagonal, diagonal(Board, Diagonal), Diagonals).

diagonal(Board, Diagonal) :-
    nth1(StartRow, Board, _),
    nth1(StartCol, [1,2,3,4,5,6,7], _),
    extract_diagonal(Board, StartRow, StartCol, Diagonal).

extract_diagonal(_, Row, Col, []) :-
    Row < 1 ; Col < 1 ; Row > 6 ; Col > 7, !.
extract_diagonal(Board, Row, Col, [Elem|Diagonal]) :-
    nth1(Row, Board, Line),
    nth1(Col, Line, Elem),
    NextRow is Row + 1,
    NextCol is Col + 1,
    extract_diagonal(Board, NextRow, NextCol, Diagonal).

consecutive_four(List, Mark) :-
    append(_, [Mark, Mark, Mark, Mark|_], List).

% Vérifie si une colonne est valide
valid_column(Board, Col) :-
    Col >= 1, Col =< 7,
    nth1(1, Board, TopRow),
    nth1(Col, TopRow, e).

% Boucle principale
play :-
    init_board(Board),
    display_board(Board),
    play_turn(Board, x).

% Tour de jeu
play_turn(Board, Player) :-
    format('Player ~w, choose a column (1-7): ', [Player]),
    read(Col),
    (   valid_column(Board, Col)
    ->  insert_in_column(Board, Col, Player, NewBoard),
        display_board(NewBoard),
        (   win(NewBoard, Player)
        ->  format('Player ~w wins!~n', [Player])
        ;   next_player(Player, NextPlayer),
            play_turn(NewBoard, NextPlayer)
        )
    ;   write('Invalid move. Try again.'), nl,
        play_turn(Board, Player)
    ).

% Inverse le joueur
next_player(x, o).
next_player(o, x).

% Transpose une matrice
transpose([[]|_], []) :- !.
transpose(Matrix, [Col|Cols]) :-
    maplist(head_tail, Matrix, Col, RestMatrix),
    transpose(RestMatrix, Cols).

head_tail([H|T], H, T).
