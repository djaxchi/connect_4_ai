%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%         PUISSANCE 4 avec IA Alpha-Beta            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% --------------------------------
%  1- INITIALISATION DU PLATEAU
% --------------------------------

:- dynamic
    total_time_x/1,
    total_time_o/1,
    moves_x/1,
    moves_o/1.

reset_ia_timers :-
    retractall(total_time_x(_)),
    retractall(total_time_o(_)),
    retractall(moves_x(_)),
    retractall(moves_o(_)),
    asserta(total_time_x(0.0)),
    asserta(total_time_o(0.0)),
    asserta(moves_x(0)),
    asserta(moves_o(0)).


store_time_for_ia(x, Duration) :-
    retract(total_time_x(T0)),
    T1 is T0 + Duration,
    asserta(total_time_x(T1)),

    retract(moves_x(M0)),
    M1 is M0 + 1,
    asserta(moves_x(M1)).

store_time_for_ia(o, Duration) :-
    retract(total_time_o(T0)),
    T1 is T0 + Duration,
    asserta(total_time_o(T1)),

    retract(moves_o(M0)),
    M1 is M0 + 1,
    asserta(moves_o(M1)).

report_ia_times :-
    total_time_x(Tx), moves_x(Mx),
    total_time_o(To), moves_o(Mo),
    ( Mx > 0 ->
        AvgX is Tx / Mx
      ; AvgX = 0
    ),
    ( Mo > 0 ->
        AvgO is To / Mo
      ; AvgO = 0
    ),
    format("IA X => total time: ~2f s, moves: ~d, avg per move: ~2f s~n", [Tx, Mx, AvgX]),
    format("IA O => total time: ~2f s, moves: ~d, avg per move: ~2f s~n", [To, Mo, AvgO]).

init_board(Board) :-
    Board = [[e, e, e, e, e, e, e],
             [e, e, e, e, e, e, e],
             [e, e, e, e, e, e, e],
             [e, e, e, e, e, e, e],
             [e, e, e, e, e, e, e],
             [e, e, e, e, e, e, e]].

% -------------------------------
%  2- AFFICHAGE DU PLATEAU
% -------------------------------

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
    length(Board, NumCols),
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

valid_column(Board, Col) :-
    between(1, 7, Col),
    transpose(Board, Transposed),
    nth1(Col, Transposed, Column),
    member(e, Column).

next_player(x, o).
next_player(o, x).

% ---------------------------------------
%  6b - Compter le nombre de pions posés
% ---------------------------------------
count_filled_cells(Board, Count) :-
    flatten(Board, Flattened),
    exclude(=(e), Flattened, Occupied),
    length(Occupied, Count).

% -------------------------------
%  7- BOUCLE PRINCIPALE DE JEU
% -------------------------------

play :-
    write('How many human players? (0, 1, or 2): '),
    read(NumPlayers),
    (   NumPlayers = 0
    ->  write('Difficulty for AI (X) (1: Easy, 2: Medium, 3: Hard, 4: Strategic): '),
        read(DifficultyX),
        write('Difficulty for AI (O) (1: Easy, 2: Medium, 3: Hard, 4: Strategic): '),
        read(DifficultyO),
        init_board(Board),
        display_board(Board),
        play_turn_2_ais(Board, x, DifficultyX, DifficultyO)
    ;   NumPlayers = 1
    ->  write('Choose AI difficulty (1: Easy, 2: Medium, 3: Hard, 4: Strategic AI): '),
        read(Difficulty),
        init_board(Board),
        display_board(Board),
        play_turn_1_human(Board, x, Difficulty)
    ;   NumPlayers = 2
    ->  init_board(Board),
        display_board(Board),
        play_turn_2_humans(Board, x)
    ;   writeln('Invalid choice, please enter 0, 1, or 2.'),
        play
    ).

% Tour humain vs IA
play_turn_1_human(Board, Player, Difficulty) :-
    (   Player = x
    ->  format('Player ~w, choose a column (1-7): ', [x]),
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
    ;   ai_move(Board, o, NewBoard, Difficulty),
        display_board(NewBoard),
        (   win(NewBoard, o)
        ->  writeln('AI (O) wins!')
        ;   next_player(o, NextPlayer),
            play_turn_1_human(NewBoard, NextPlayer, Difficulty)
        )
    ).

% Tour 2 joueurs humains
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

% Tour IA vs IA
play_turn_2_ais(Board, Player, DifficultyX, DifficultyO) :-
    (   Player = x
    ->  ai_move(Board, x, NewBoard, DifficultyX),
        display_board(NewBoard),
        (   win(NewBoard, x)
        ->  writeln('AI (X) wins!')
        ;   next_player(x, NextPlayer),
            play_turn_2_ais(NewBoard, NextPlayer, DifficultyX, DifficultyO)
        )
    ;   ai_move(Board, o, NewBoard, DifficultyO),
        display_board(NewBoard),
        (   win(NewBoard, o)
        ->  writeln('AI (O) wins!')
        ;   next_player(o, NextPlayer),
            play_turn_2_ais(NewBoard, NextPlayer, DifficultyX, DifficultyO)
        )
    ).

% play_turn_2_ais(+Board, +Player, +DifficultyX, +DifficultyO, -Winner)
% Gère une partie entre deux IA, unifie Winner à x ou o.
play_turn_2_ais(Board, Player, DiffX, DiffO, Winner) :-
    (   Player = x
    ->  ai_move(Board, x, NewBoard, DiffX),
        (   win(NewBoard, x)
        ->  Winner = x
        ;   next_player(x, NextPlayer),
            play_turn_2_ais(NewBoard, NextPlayer, DiffX, DiffO, Winner)
        )
    ;   ai_move(Board, o, NewBoard, DiffO),
        (   win(NewBoard, o)
        ->  Winner = o
        ;   next_player(o, NextPlayer),
            play_turn_2_ais(NewBoard, NextPlayer, DiffX, DiffO, Winner)
        )
    ).

    % one_game_ai_vs_ai(+DiffX, +DiffO, -Winner)
% Lance une partie "IA X vs IA O" avec les difficultés spécifiées, renvoie x ou o (ou draw).
one_game_ai_vs_ai(DiffX, DiffO, Winner) :-
    init_board(Board),
    play_turn_2_ais(Board, x, DiffX, DiffO, Winner).


% run_n_games(+DiffX, +DiffO, +N, -WinsX, -WinsO, -Draws, -TotalTime)
% Lance N parties IA(X) vs IA(O). Retourne stats + temps cumulé.
run_n_games(_, _, 0, 0, 0, 0, 0) :- !.  % plus de parties à jouer

run_n_games(DiffX, DiffO, N, WinsX, WinsO, Draws, TotalTime) :-
    N > 0,
    get_time(Start),
    one_game_ai_vs_ai(DiffX, DiffO, Winner),
    get_time(End),
    Elapsed is End - Start,
    (   Winner = x
    ->  Wx = 1, Wo = 0, Dr = 0
    ;   Winner = o
    ->  Wx = 0, Wo = 1, Dr = 0
    ;   % si tu gères un match nul
        Wx = 0, Wo = 0, Dr = 1
    ),
    N1 is N - 1,
    run_n_games(DiffX, DiffO, N1, Wx2, Wo2, Dr2, T2),
    WinsX is Wx + Wx2,
    WinsO is Wo + Wo2,
    Draws is Dr + Dr2,
    TotalTime is Elapsed + T2.

benchmark_ai_vs_ai(DiffX, DiffO, NumGames) :-
    % Réinitialiser les compteurs IA
    reset_ia_timers,

    % Lancer N parties, accumuler victoires et temps global
    run_n_games(DiffX, DiffO, NumGames, WinsX, WinsO, Draws, TotalTime),

    % Affiche les stats globales
    writeln("-----------------------------------------------------"),
    format("Benchmark Done: ~d games between X=~w and O=~w~n", [NumGames, DiffX, DiffO]),
    format("X wins=~d, O wins=~d, draws=~d, total time=~2f s~n", [WinsX, WinsO, Draws, TotalTime]),
    ( NumGames > 0 -> AvgTime is TotalTime / NumGames ; AvgTime = 0 ),
    format("Average time per game: ~2f s~n", [AvgTime]),

    % Temps de réflexion moyen pour chaque IA
    report_ia_times,
    writeln("-----------------------------------------------------").


% ---------------------------------
%  8- IA : 3 (ou 4) NIVEAUX DE DIFFICULTÉ
% ---------------------------------

% Niveau 1 : Aléatoire
ai_move(Board, Player, NewBoard, 1) :-
    random_ai_move(Board, Player, NewBoard).

% Niveau 2 : Medium => Alpha-Beta (Depth=2) with Basic Eval
ai_move(Board, Player, NewBoard, 2) :-
    best_move_alpha_beta(Board, Player, 2, BestCol, 0),  % 0 => basic evaluation
    (   BestCol == nil
    ->  writeln('No valid moves! Playing randomly.'),
        random_ai_move(Board, Player, NewBoard)
    ;   format('AI chose column ~d~n', [BestCol]),
        insert_in_column(Board, BestCol, Player, NewBoard),
        get_time(End),
        Elapsed is End - Start,
        store_time_for_ia(Player, Elapsed).
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IA DIFFICULTE 3 : 1) Premiers coups => [3,4,5] aléatoire %
%                     2) Ensuite => Depth agressif         %
%                        - si Count <12 => Depth=2         %
%                        - si Count <25 => Depth=3         %
%                        - else        => Depth=5          %
%                     3) Fallback random si plus de coups  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ai_move(Board, Player, NewBoard, 3) :-
    % Clear cached evaluations
    % Count how many cells are filled on the board
    count_filled_cells(Board, Count),
    (
        % 1 If fewer than 3 cells are filled => random among cols [3,4,5]
        Count < 3 ->
            random_central_move_3(Board, Player, NewBoard)
        ;
        % 2 Otherwise => apply the "aggressive depth reduction"
        (
            Count < 6 -> Depth = 2     % early game
          ; Count < 30 -> Depth = 3     % mid game
          ;               Depth = 5     % late game
        ),
        % Call alpha-beta with advanced evaluation (Eval=1)
        best_move_alpha_beta(Board, Player, Depth, BestCol, 1),
        (   BestCol == nil
        ->  writeln('No valid moves! Playing randomly.'),
            random_ai_move(Board, Player, NewBoard)
        ;   format('AI (Hard) chose column ~d (Depth=~d)~n', [BestCol, Depth]),
            insert_in_column(Board, BestCol, Player, NewBoard),
            get_time(End),
            Elapsed is End - Start,
            store_time_for_ia(Player, Elapsed).
        )
    ).

%
% random_central_move_3/3
%  - Chooses randomly among columns [3,4,5] if valid
%  - If none is valid, fallback to the alpha-beta logic above
%
random_central_move_3(Board, Player, NewBoard) :-
    findall(Col,
        (   member(Col, [3,4,5]),
            valid_column(Board, Col)
        ),
        ValidCentralCols
    ),
    (
        ValidCentralCols = []
        ->  % If no central columns valid, we do a fallback:
            % Use the same "aggressive depth" approach
            count_filled_cells(Board, Count),
            (
                Count < 12 -> Depth = 2
              ; Count < 25 -> Depth = 3
              ;               Depth = 5
            ),
            best_move_alpha_beta(Board, Player, Depth, BestCol, 1),
            (   BestCol == nil
            ->  writeln('No valid moves! Playing randomly.'),
                random_ai_move(Board, Player, NewBoard)
            ;   format('AI (Hard fallback) chose column ~d (Depth=~d)~n', [BestCol, Depth]),
                insert_in_column(Board, BestCol, Player, NewBoard),
                
            )
        ;   % Otherwise pick one of the central columns randomly
            random_member(ChosenCol, ValidCentralCols),
            format('AI (Hard) first moves, picks among [3,4,5], chooses column ~d~n', [ChosenCol]),
            insert_in_column(Board, ChosenCol, Player, NewBoard)
    ).

%
% count_filled_cells/2
%  - Count how many cells in the board are not 'e'
%
count_filled_cells(Board, Count) :-
    findall(Cell,
            (
                member(Row, Board),
                member(Cell, Row),
                Cell \= e
            ),
            Cells),
    length(Cells, Count).

% Niveau 4 : "Strategic" => More Aggressive Depth with "Eval=2"
ai_move(Board, Player, NewBoard, 4) :-
    count_filled_cells(Board, Count),
    (   Count < 12 -> Depth = 3  % early => Depth=3
    ;   Count < 25 -> Depth = 4  % mid => Depth=4
    ;   Depth = 6               % late => Depth=6
    ),
    best_move_alpha_beta(Board, Player, Depth, BestCol, 2),  % 2 => new heuristic
    (   BestCol == nil
    ->  writeln('No valid moves! Playing randomly.'),
        random_ai_move(Board, Player, NewBoard)
    ;   format('AI (Eval=2) chose column ~d (Depth=~d)~n', [BestCol, Depth]),
        insert_in_column(Board, BestCol, Player, NewBoard)
        get_time(End),
        Elapsed is End - Start
        store_time_for_ia(Player, Elapsed).
    ).

% ---------------------------------
%  9- IA ALÉATOIRE
% ---------------------------------

random_ai_move(Board, Player, NewBoard) :-
    findall(Col, valid_column(Board, Col), ValidMoves),
    ValidMoves \= [],
    random_member(Col, ValidMoves),
    format('AI chose column ~d~n', [Col]),
    insert_in_column(Board, Col, Player, NewBoard).

% ---------------------------------
% 10- TRANSPOSE
% ---------------------------------

transpose([[]|_], []) :- !.
transpose(Matrix, [Col|Cols]) :-
    maplist(head_tail, Matrix, Col, RestMatrix),
    transpose(RestMatrix, Cols).

head_tail([H|T], H, T).

% ---------------------------------
% 11- ALPHA-BETA : IMPLEMENTATION
% ---------------------------------

%%% Evaluate with caching (prints debug)
evaluate_board(Board, Player, Score, Eval) :-
    term_to_atom(Board, Key),  % Convert board to a unique key
    (  
    ;   compute_board_score(Board, Player, Score, Eval),
    ).


compute_board_score(Board, Player, Score, Eval) :-
    (   Eval = 0 -> score_basic(Board, Player, Score)
    ;   Eval = 1 -> score_advanced(Board, Player, Score)
    ;   Eval = 2 -> score_strategic(Board, Player, Score)
    ).

% --- Basic scoring
score_basic(Board, Player, Score) :-
    opponent(Player, Opponent),
    findall(S, (line(Board, L), score_line(L, Player, Opponent, S)), Scores),
    sum_list(Scores, Score).

% --- Advanced scoring
score_advanced(Board, Player, Score) :-
    opponent(Player, Opponent),
    findall(S, (line(Board, L), score_advanced_line(L, Player, Opponent, S)), Scores),
    sum_list(Scores, Score).

% --- New "Strategic" scoring
score_strategic(Board, Player, Score) :-
    opponent(Player, Opponent),
    findall(S, (line(Board, L), score_new_heuristic(L, Player, Opponent, S)), Scores),
    sum_list(Scores, RawScore),
    position_bonus(Board, Player, PosScore),
    Score is RawScore + PosScore.

% Extract possible lines
line(Board, Line) :- member(Line, Board).  % Rows
line(Board, Line) :-
    transpose(Board, T), member(Line, T).  % Columns
line(Board, Line) :-
    diagonals_desc(Board, Ds), member(Line, Ds).  % Desc diag
line(Board, Line) :-
    diagonals_asc(Board, As), member(Line, As).   % Asc diag

% Basic line scoring
score_line(Line, Player, Opponent, Score) :-
    (   consecutive_four(Line, Player)   -> Score is 1000
    ;   consecutive_four(Line, Opponent) -> Score is -1000
    ;   three_in_a_row(Line, Player)     -> Score is 50
    ;   three_in_a_row(Line, Opponent)   -> Score is -50
    ;   two_in_a_row(Line, Player)       -> Score is 10
    ;   two_in_a_row(Line, Opponent)     -> Score is -10
    ;   Score is 0
    ).

% Advanced line scoring
score_advanced_line(Line, Player, Opponent, Score) :-
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

% "Strategic" line scoring
score_new_heuristic(Line, Player, Opponent, Score) :-
    (   consecutive_four(Line, Player)        -> Score is 1000000   % Immediate win
    ;   consecutive_four(Line, Opponent)      -> Score is -1000000  % Must block
    ;   append(_, [Player, Player, e, Player|_], Line) -> Score is 500  % AI fork
    ;   append(_, [Opponent, Opponent, e, Opponent|_], Line) -> Score is -500
    ;   three_in_a_row(Line, Player) -> Score is 100
    ;   three_in_a_row(Line, Opponent) -> Score is -200
    ;   two_in_a_row(Line, Player) -> Score is 40
    ;   two_in_a_row(Line, Opponent) -> Score is -50
    ;   Score is 0
    ).

three_in_a_row(Line, Player) :-
    append(_, [Player, Player, Player, e|_], Line);
    append(_, [e, Player, Player, Player|_], Line).

two_in_a_row(Line, Player) :-
    append(_, [Player, Player, e, e|_], Line);
    append(_, [e, e, Player, Player|_], Line).

opponent(x, o).
opponent(o, x).

center_bonus(Board, Player, Bonus) :-
    findall(1,
        (member(Row, Board), nth1(4, Row, Cell), Cell == Player),
        Centers),
    length(Centers, Count),
    Bonus is 30 * Count.

position_value(4, 3).  % center
position_value(3, 2).
position_value(5, 2).
position_value(2, 1).
position_value(6, 1).
position_value(1, 0).
position_value(7, 0).

position_bonus(Board, Player, Score) :-
    transpose(Board, Transposed),
    findall(PosValue, (
        nth1(Col, Transposed, Column),
        nth1(_Row, Column, Player),
        position_value(Col, PosValue)
    ), PosScores),
    sum_list(PosScores, Score).

% Alpha-Beta avec tri favorisant les colonnes centrales
alpha_beta(Board, Depth, Alpha, Beta, Player, Score, Eval) :-
    terminal_state(Board, Depth, Player, Val, GameOver, Eval),
    (   GameOver == true
    ->  Score = Val
    ;   findall(Col, valid_column(Board, Col), Moves),
        (   Moves = []
        ->  evaluate_board(Board, Player, Score, Eval)  % Plateau plein => match nul ?
        ;   safe_order_moves(Board, Moves, Player, OrderedMoves, Eval),  % Trier les coups pour favoriser le centre
            alpha_beta_loop(OrderedMoves, Board, Depth, Alpha, Beta, Player, Score, Eval)
        )
    ).

% Sécurisation du tri des coups
safe_order_moves(Board, Moves, Player, OrderedMoves, Eval) :-
    catch(order_moves(Board, Moves, Player, OrderedMoves, Eval), _, OrderedMoves = Moves).

% Tri des coups en favorisant le centre et en évaluant leur potentiel
order_moves(Board, Moves, Player, OrderedMoves, Eval) :-
    findall(Score-Col, (
        member(Col, Moves),
        valid_column(Board, Col),  % Vérifier que la colonne est valide
        insert_in_column(Board, Col, Player, NewBoard),
        evaluate_board(NewBoard, Player, Score, Eval),
        center_bonus(Col, CenterWeight),
        AdjustedScore is Score + CenterWeight  % Favorise les colonnes centrales
    ), Pairs),
    sort(1, @>=, Pairs, SortedPairs),  % Trier en ordre décroissant
    pairs_values(SortedPairs, OrderedMoves).

% Bonus pour favoriser les colonnes centrales
center_bonus(Col, Bonus) :-
    (Col = 4 -> Bonus is 30;
     Col = 3; Col = 5 -> Bonus is 20;
     Col = 2; Col = 6 -> Bonus is 10;
     Col = 1; Col = 7 -> Bonus is 0).

% Boucle principale Alpha-Beta avec élagage
alpha_beta_loop([], _, _, Alpha, _, _, Alpha, _).
alpha_beta_loop([Move|Moves], Board, Depth, Alpha, Beta, Player, BestScore, Eval) :-
    insert_in_column(Board, Move, Player, NewBoard),
    next_player(Player, NextPlayer),
    NewDepth is Depth - 1,
    alpha_beta(NewBoard, NewDepth, -Beta, -Alpha, NextPlayer, ValNeg, Eval),
    Val is -ValNeg,
    (   Val >= Beta  % Coup trop bon pour ladversaire => on coupe
    ->  BestScore = Val
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

terminal_state(Board, Depth, Player, Score, true, Eval) :-
    (   win(Board, x)
    ;   win(Board, o)
    ;   Depth =< 0
    ), !,
    evaluate_board(Board, Player, Score, Eval).

terminal_state(_, Depth, _, _, false, _) :-
    Depth > 0.
