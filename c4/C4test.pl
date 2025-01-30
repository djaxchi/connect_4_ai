%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%         PUISSANCE 4 avec IA Alpha-Beta            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ------------------------------
%  1- INITIALISATION DU PLATEAU
% ------------------------------

% Initialise un plateau vide (6 lignes x 7 colonnes)
init_board(Board) :-
    Board = [[e, e, e, e, e, e, e],
             [e, e, e, e, e, e, e],
             [e, e, e, e, e, e, e],
             [e, e, e, e, e, e, e],
             [e, e, e, e, e, e, e],
             [e, e, e, e, e, e, e]].

% --------------------------------
%  2- AFFICHAGE DU PLATEAU
% --------------------------------

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

% ----------------------------------------
%  3- INSERER UN PION DANS UNE COLONNE
% ----------------------------------------

insert_in_column(Board, Col, Mark, NewBoard) :-
    transpose(Board, Transposed),
    nth1(Col, Transposed, Column),
    reverse(Column, ReversedColumn),
    replace_first_empty(ReversedColumn, Mark, UpdatedReversedColumn),
    reverse(UpdatedReversedColumn, UpdatedColumn),
    replace_column(Transposed, Col, UpdatedColumn, UpdatedTransposed),
    transpose(UpdatedTransposed, NewBoard).

replace_first_empty([e|Rest], Mark, [Mark|Rest]) :- !.
replace_first_empty([Cell|Rest], Mark, [Cell|UpdatedRest]) :-
    replace_first_empty(Rest, Mark, UpdatedRest).

replace_column([Col|Cols], 1, NewCol, [NewCol|Cols]) :- !.
replace_column([Col|Cols], Index, NewCol, [Col|UpdatedCols]) :-
    Index > 1,
    NextIndex is Index - 1,
    replace_column(Cols, NextIndex, NewCol, UpdatedCols).

% ------------------------
%  4- TEST DE LA VICTOIRE
% ------------------------

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

consecutive_four(List, Mark) :-
    append(_, [Mark, Mark, Mark, Mark|_], List).

% --------------------------
%  5- EXTRACTION DIAGONALES
% --------------------------

diagonals_desc(Board, Diagonals) :-
    findall(Diagonal, diagonal_desc(Board, Diagonal), Diagonals).

diagonal_desc(Board, Diagonal) :-
    length(Board, NumRows),
    length(Board, NumCols),  % en réalité 7, mais on laisse flexible
    (   between(1, NumRows, StartRow), StartCol = 1
    ;   StartRow = 1, between(2, NumCols, StartCol)
    ),
    extract_diagonal_desc(Board, StartRow, StartCol, Diagonal).

extract_diagonal_desc(_, Row, Col, []) :-
    Row > 6 ; Col > 7, !.
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
    (   between(1, NumRows, StartRow), StartCol = 1
    ;   StartRow = NumRows, between(2, NumCols, StartCol)
    ),
    extract_diagonal_asc(Board, StartRow, StartCol, Diagonal).

extract_diagonal_asc(_, Row, Col, []) :-
    Row < 1 ; Col > 7, !.
extract_diagonal_asc(Board, Row, Col, [Elem|Diagonal]) :-
    nth1(Row, Board, Line),
    nth1(Col, Line, Elem),
    NextRow is Row - 1,
    NextCol is Col + 1,
    extract_diagonal_asc(Board, NextRow, NextCol, Diagonal).

% -------------------------------------
%  6- VALIDITE DE LA COLONNE + TOUR DE JEU
% -------------------------------------

% Vérifie si une colonne est valide (entre 1 et 7, et qu’elle contient au moins une case vide)
valid_column(Board, Col) :-
    nonvar(Board),
    between(1, 7, Col),
    transpose(Board, Transposed),
    nth1(Col, Transposed, Column),
    member(e, Column).

% Inverse le joueur
next_player(x, o).
next_player(o, x).

% -------------------------------
%  7- BOUCLE PRINCIPALE DE JEU
% -------------------------------

play :-
    write('How many human players? (0, 1, or 2): '),
    read(NumPlayers),
    (   NumPlayers = 0
    ->  % IA vs IA
        write('Difficulty for AI (X) (1: Easy, 2: Medium, 3: Hard): '),
        read(DifficultyX),
        write('Difficulty for AI (O) (1: Easy, 2: Medium, 3: Hard): '),
        read(DifficultyO),
        init_board(Board),
        display_board(Board),
        play_turn_2_ais(Board, x, DifficultyX, DifficultyO)
    ;   NumPlayers = 1
    ->  % 1 joueur humain contre une IA
        write('Choose AI difficulty (1: Easy, 2: Medium, 3: Hard, 4: Strategic AI): '),
        read(Difficulty),
        init_board(Board),
        display_board(Board),
        play_turn_1_human(Board, x, Difficulty)
    ;   NumPlayers = 2
    ->  % 2 joueurs humains
        init_board(Board),
        display_board(Board),
        play_turn_2_humans(Board, x)
    ;   % Sinon, entrée invalide
        writeln('Invalid choice, please enter 0, 1, or 2.'),
        play
    ).


% play_turn_1_human(+Board, +Player, +Difficulty)
% Gère une partie : 
% - Player x = Humain
% - Player o = IA avec Difficulty
play_turn_1_human(Board, Player, Difficulty) :-
    (   Player = x
    ->  % Tour du joueur humain
        format('Player ~w, choose a column (1-7): ', [x]),
        read(Col),
        (   valid_column(Board, Col)
        ->  insert_in_column(Board, Col, x, NewBoard),
            display_board(NewBoard),
            (   win(NewBoard, x)
            ->  writeln('Player X wins!')
            ;   next_player(x, NextPlayer),
                play_turn_1_human(NewBoard, NextPlayer, Difficulty)
            )
        ;   writeln('Invalid move. Try again.'),
            play_turn_1_human(Board, x, Difficulty)
        )
    ;   % Tour de l'IA
        ai_move(Board, o, NewBoard, Difficulty),
        display_board(NewBoard),
        (   win(NewBoard, o)
        ->  writeln('AI (O) wins!')
        ;   next_player(o, NextPlayer),
            play_turn_1_human(NewBoard, NextPlayer, Difficulty)
        )
    ).


% play_turn_2_humans(+Board, +Player)
% Gère une partie avec deux joueurs humains.
play_turn_2_humans(Board, Player) :-
    format('Player ~w, choose a column (1-7): ', [Player]),
    read(Col),
    (   valid_column(Board, Col)
    ->  insert_in_column(Board, Col, Player, NewBoard),
        display_board(NewBoard),
        (   win(NewBoard, Player)
        ->  format('Player ~w wins!~n', [Player])
        ;   next_player(Player, NextPlayer),
            play_turn_2_humans(NewBoard, NextPlayer)
        )
    ;   writeln('Invalid move. Try again.'),
        play_turn_2_humans(Board, Player)
    ).

% play_turn_2_ais(+Board, +Player, +DifficultyX, +DifficultyO)
% Gère une partie entre deux IA (x et o) avec deux niveaux de difficulté différents.
play_turn_2_ais(Board, Player, DifficultyX, DifficultyO) :-
    (   Player = x
    ->  % IA X joue avec sa difficulté
        ai_move(Board, x, NewBoard, DifficultyX),
        display_board(NewBoard),
        (   win(NewBoard, x)
        ->  writeln('AI (X) wins!')
        ;   next_player(x, NextPlayer),
            play_turn_2_ais(NewBoard, NextPlayer, DifficultyX, DifficultyO)
        )
    ;   % IA O joue avec sa difficulté
        ai_move(Board, o, NewBoard, DifficultyO),
        display_board(NewBoard),
        (   win(NewBoard, o)
        ->  writeln('AI (O) wins!')
        ;   next_player(o, NextPlayer),
            play_turn_2_ais(NewBoard, NextPlayer, DifficultyX, DifficultyO)
        )
    ).


% ---------------------------------
%  8- IA : 3 NIVEAUX DE DIFFICULTÉ
% ---------------------------------

% Niveau 1 : Aléatoire
ai_move(Board, Player, NewBoard, 1) :-
    random_ai_move(Board, Player, NewBoard).

% Niveau 2 : Medium => Alpha-Beta avec profondeur réduite (ex : 3)
ai_move(Board, Player, NewBoard, 2) :-
    Depth = 3,
    best_move_alpha_beta(Board, Player, Depth, BestCol, 0),  % 0 => "basic" evaluation
    (   BestCol == nil
    ->  writeln('No valid moves! Playing randomly.'),
        random_ai_move(Board, Player, NewBoard)
    ;   format('AI chose column ~d~n', [BestCol]),
        insert_in_column(Board, BestCol, Player, NewBoard)
    ).

% Niveau 3 : Hard => Alpha-Beta avec profondeur plus élevée (ex : 5)
ai_move(Board, Player, NewBoard, 3) :-
    Depth = 5,
    best_move_alpha_beta(Board, Player, Depth, BestCol, 1),  % 1 => "advanced" evaluation
    (   BestCol == nil
    ->  writeln('No valid moves! Playing randomly.'),
        random_ai_move(Board, Player, NewBoard)
    ;   format('AI chose column ~d~n', [BestCol]),
        insert_in_column(Board, BestCol, Player, NewBoard)
    ).

ai_move(Board, Player, NewBoard, 4) :-
    Depth = 6,  % Slightly deeper search for Eval=2
    best_move_alpha_beta(Board, Player, Depth, BestCol, 2),  % 2 => New evaluation
    (   BestCol == nil
    ->  writeln('No valid moves! Playing randomly.'),
        random_ai_move(Board, Player, NewBoard)
    ;   format('AI (Eval=2) chose column ~d~n', [BestCol]),
        insert_in_column(Board, BestCol, Player, NewBoard)
    ).

% ---------------------------------
%  9- IA ALÉATOIRE
% ---------------------------------

% IA aléatoire : Joue dans une colonne valide choisie au hasard
random_ai_move(Board, Player, NewBoard) :-
    findall(Col, valid_column(Board, Col), ValidMoves),
    ValidMoves \= [],
    random_member(Col, ValidMoves),
    format('AI chose column ~d~n', [Col]),
    insert_in_column(Board, Col, Player, NewBoard).

% ---------------------------------
% 10- TRANSPOSE (utile un peu partout)
% ---------------------------------

transpose([[]|_], []) :- !.
transpose(Matrix, [Col|Cols]) :-
    maplist(head_tail, Matrix, Col, RestMatrix),
    transpose(RestMatrix, Cols).

head_tail([H|T], H, T).

% ---------------------------------
% 11- ALPHA-BETA : IMPLEMENTATION
% ---------------------------------

% Two evaluations in one predicate, controlled by 'Eval' (0 or 1):
evaluate_board(Board, Player, Score, Eval) :-
    (  % Basic evaluation if Eval=0
       Eval = 0,
       opponent(Player, Opponent),
       findall(S, (line(Board, L), score_line(L, Player, Opponent, S)), Scores),
       sum_list(Scores, Score)
    ;  % Advanced evaluation if Eval=1
       Eval = 1,
       next_player(Player, Opponent),
       findall(S, (
           (member(Row, Board), score_advanced(Row, Player, Opponent, S));
           (transpose(Board, Transposed), member(Column, Transposed), score_advanced(Column, Player, Opponent, S));
           (diagonals_desc(Board, DescDiags), member(Diag, DescDiags), score_advanced(Diag, Player, Opponent, S));
           (diagonals_asc(Board, AscDiags), member(Diag, AscDiags), score_advanced(Diag, Player, Opponent, S))
       ), Scores),
       center_bonus(Board, Player, CenterBonus),
       sum_list(Scores, RawScore),
       Score is RawScore + CenterBonus
    ;  % New evaluation if Eval=2 (Center Control + Threat Prioritization)
       Eval = 2,
       next_player(Player, Opponent),
       findall(S, (
           (line(Board, L), score_new_heuristic(L, Player, Opponent, S))
       ), Scores),
       sum_list(Scores, RawScore),
       position_bonus(Board, Player, PosScore),
       Score is RawScore + PosScore
    ).

% Get all possible lines (rows, columns, diagonals)
line(Board, Line) :- member(Line, Board). % Rows
line(Board, Line) :-
    transpose(Board, Transposed),
    member(Line, Transposed). % Columns
line(Board, Line) :-
    diagonals_desc(Board, Diags),
    member(Line, Diags).      % Descending diagonals
line(Board, Line) :-
    diagonals_asc(Board, Diags),
    member(Line, Diags).      % Ascending diagonals

% Simple scoring for basic evaluation
score_line(Line, Player, Opponent, Score) :-
    (   consecutive_four(Line, Player)   -> Score is 1000
    ;   consecutive_four(Line, Opponent) -> Score is -1000
    ;   three_in_a_row(Line, Player)     -> Score is 50
    ;   three_in_a_row(Line, Opponent)   -> Score is -50
    ;   two_in_a_row(Line, Player)       -> Score is 10
    ;   two_in_a_row(Line, Opponent)     -> Score is -10
    ;   Score is 0
    ).

% Check if there are three pieces in a row with one empty spot
three_in_a_row(Line, Player) :-
    append(_, [Player, Player, Player, e|_], Line).
three_in_a_row(Line, Player) :-
    append(_, [e, Player, Player, Player|_], Line).

% Check if there are two pieces in a row with two empty spots
two_in_a_row(Line, Player) :-
    append(_, [Player, Player, e, e|_], Line).
two_in_a_row(Line, Player) :-
    append(_, [e, e, Player, Player|_], Line).

% Opponent
opponent(x, o).
opponent(o, x).

% Advanced scoring
score_advanced(Line, Player, Opponent, Score) :-
    (   consecutive_four(Line, Player)       -> Score is 10000
    ;   consecutive_four(Line, Opponent)     -> Score is -10000
    ;   three_in_a_row_with_space(Line, Player)   -> Score is 200
    ;   three_in_a_row_with_space(Line, Opponent) -> Score is -250
    ;   two_in_a_row_with_space(Line, Player)     -> Score is 50
    ;   two_in_a_row_with_space(Line, Opponent)   -> Score is -60
    ;   Score is 0
    ).

three_in_a_row_with_space(Line, Mark) :-
    append(_, [Mark, Mark, Mark, e|_], Line);
    append(_, [e, Mark, Mark, Mark|_], Line).

two_in_a_row_with_space(Line, Mark) :-
    append(_, [Mark, Mark, e, e|_], Line);
    append(_, [e, e, Mark, Mark|_], Line).

% Bonus for central column placement
center_bonus(Board, Player, Bonus) :-
    % Summation of each row's center cell, or a simpler approach:
    findall(1,
        (member(Row, Board), nth1(4, Row, Cell), Cell == Player),
        Centers),
    length(Centers, Count),
    % e.g. +30 per piece in the center column
    Bonus is 30 * Count.

score_new_heuristic(Line, Player, Opponent, Score) :-
    (   consecutive_four(Line, Player) -> Score is 1000000  % Immediate win
    ;   consecutive_four(Line, Opponent) -> Score is -1000000  % Block loss
    ;   append(_, [Player, Player, e, Player|_], Line) -> Score is 500  % AI fork setup
    ;   append(_, [Opponent, Opponent, e, Opponent|_], Line) -> Score is -500  % Opponent fork setup
    ;   three_in_a_row(Line, Player) -> Score is 100  % Near-win
    ;   three_in_a_row(Line, Opponent) -> Score is -200  % Blocking is more important
    ;   two_in_a_row(Line, Player) -> Score is 40  % Strong setup
    ;   two_in_a_row(Line, Opponent) -> Score is -50  % Stop opponent setup
    ;   Score is 0
    ).

% Center columns are more valuable
position_value(4, 3).  % Center column (best spot)
position_value(3, 2).  % Slightly valuable
position_value(5, 2).
position_value(2, 1).  % Less valuable
position_value(6, 1).
position_value(1, 0).  % Edge (least valuable)
position_value(7, 0).

% Calculate position bonus
position_bonus(Board, Player, Score) :-
    transpose(Board, Transposed),
    findall(PosValue, (
        nth1(Col, Transposed, Column),
        nth1(Row, Column, Player),  % Find player's pieces
        position_value(Col, PosValue)
    ), PosScores),
    sum_list(PosScores, Score).


% terminal_state : vrai si état terminal OU profondeur épuisée
terminal_state(Board, Depth, Player, Score, true, Eval) :-
    (   win(Board, x)
    ;   win(Board, o)
    ;   Depth =< 0
    ), !,
    evaluate_board(Board, Player, Score, Eval).

terminal_state(_, Depth, _, _, false, _) :-
    Depth > 0.

% alpha_beta(+Board, +Depth, +Alpha, +Beta, +Player, -Score, +Eval)
alpha_beta(Board, Depth, Alpha, Beta, Player, Score, Eval) :-
    terminal_state(Board, Depth, Player, Val, GameOver, Eval),
    (   GameOver == true
    ->  Score = Val
    ;   findall(Col, valid_column(Board, Col), Moves),
        (   Moves = []
        ->  % pas de coups => plateau plein => match nul ?
            evaluate_board(Board, Player, Score, Eval)
        ;   alpha_beta_loop(Moves, Board, Depth, Alpha, Beta, Player, Score, Eval)
        )
    ).

% Parcours des coups valides (NegaMax + élagage)
alpha_beta_loop([], _, _, Alpha, _, _, Alpha, _).
alpha_beta_loop([Move|Moves], Board, Depth, Alpha, Beta, Player, BestScore, Eval) :-
    insert_in_column(Board, Move, Player, NewBoard),
    next_player(Player, NextPlayer),
    NewDepth is Depth - 1,
    alpha_beta(NewBoard, NewDepth, -Beta, -Alpha, NextPlayer, ValNeg, Eval),
    Val is -ValNeg,
    (   Val >= Beta
    ->  BestScore = Val  % Élagage
    ;   NewAlpha is max(Alpha, Val),
        alpha_beta_loop(Moves, Board, Depth, NewAlpha, Beta, Player, BestScore, Eval)
    ).

% Sélection du meilleur coup
best_move_alpha_beta(Board, Player, Depth, BestCol, Eval) :-
    findall(Col, valid_column(Board, Col), ValidMoves),
    (   ValidMoves = []
    ->  BestCol = nil
    ;   best_move_loop(ValidMoves, Board, Player, Depth, -100000, nil, BestCol, Eval)
    ).

% best_move_loop/8 : on teste chaque coup, on garde le meilleur
best_move_loop([], _, _, _, _, ColAcc, ColAcc, _).
best_move_loop([Move|Moves], Board, Player, Depth, BestScoreSoFar, BestColSoFar, BestCol, Eval) :-
    insert_in_column(Board, Move, Player, NewBoard),
    next_player(Player, NextPlayer),
    NewDepth is Depth - 1,
    alpha_beta(NewBoard, NewDepth, -100000, 100000, NextPlayer, ValNeg, Eval),
    Score is -ValNeg,
    (   Score > BestScoreSoFar
    ->  NewBestScore = Score,
        NewBestCol   = Move
    ;   NewBestScore = BestScoreSoFar,
        NewBestCol   = BestColSoFar
    ),
    best_move_loop(Moves, Board, Player, Depth, NewBestScore, NewBestCol, BestCol, Eval).
