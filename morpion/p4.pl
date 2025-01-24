%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CST 381 -– Artificial Intelligence
%%% Robert Pinchbeck
%%% Final Project 
%%% Due December 20, 2006
%%% Source : http://www.robertpinchbeck.com/college/work/prolog/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% A Prolog Implementation of Tic-Tac-Toe
%%% using the minimax strategy
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*

The following conventions are used in this program...

Single letter variables represent:

L - a list
N - a number, position, index, or counter
V - a value (usually a string)
A - an accumulator
H - the head of a list
T - the tail of a list

For this implementation, these single letter variables represent:

P - a player number (1 or 2)
B - the board (a 9 item list representing a 3x3 matrix)
    each "square" on the board can contain one of 3 values: x ,o, or e (for empty)
S - the number of a square on the board (1 - 9)
M - a mark on a square (x or o)
E - the mark used to represent an empty square ('e').
U - the utility value of a board position
R - a random number
D - the depth of the minimax search tree (for outputting utility values, and for debugging)

Variables with a numeric suffix represent a variable based on another variable.
(e.g. B2 is a new board position based on B)

For predicates, the last variable is usually the "return" value.
(e.g. opponent_mark(P,M), returns the opposing mark in variable M)

Predicates with a numeric suffix represent a "nested" predicate.

e.g. myrule2(...) is meant to be called from myrule(...) 
     and myrule3(...) is meant to be called from myrule2(...)


There are only two assertions that are used in this implementation

asserta( board(B) ) - the current board 
asserta( player(P, Type) ) - indicates which players are human/computer.

*/



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%     FACTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

next_player(1, 2).      %%% determines the next player after the given player
next_player(2, 1).

inverse_mark('x', 'o'). %%% determines the opposite of the given mark
inverse_mark('o', 'x').

player_mark(1, 'x').    %%% the mark for the given player
player_mark(2, 'o').    

opponent_mark(1, 'o').  %%% shorthand for the inverse mark of the given player
opponent_mark(2, 'x').

blank_mark('e').        %%% the mark used in an empty square

maximizing('x').        %%% the player playing x is always trying to maximize the utility of the board position
minimizing('o').        %%% the player playing o is always trying to minimize the utility of the board position

corner_square(1, 1).    %%% map corner squares to board squares
corner_square(2, 3).
corner_square(3, 7).
corner_square(4, 9).


my_member(X, [X|_]).  % Pas de coupure ici
my_member(X, [_|T]) :-
    my_member(X, T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%     MAIN PROGRAM
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


run :-
    hello,          %%% Display welcome message, initialize game

    play(1),        %%% Play the game starting with player 1

    goodbye         %%% Display end of game message
    .

run :-
    goodbye
    .


hello :-
    initialize,
%    cls,
    nl,
    nl,
    nl,
    write('Welcome to Connect-4!'),
    read_players,
    output_players
    .

initialize :-
    retractall(board(_)), 
    random_seed,          %%% use current time to initialize random number generator
    blank_mark(E),
    asserta(board([[E, E, E, E, E, E, E],
                   [E, E, E, E, E, E, E],
                   [E, E, E, E, E, E, E],
                   [E, E, E, E, E, E, E],
                   [E, E, E, E, E, E, E],
                   [E, E, E, E, E, E, E]])).

goodbye :-
    board(B),
    nl,
    nl,
    write('Game over: '),
    output_winner(B),
    retract(board(_)),
    retract(player(_,_)),
    read_play_again(V), !,
    (V == 'Y' ; V == 'y'), 
    !,
    run
    .

read_play_again(V) :-
    nl,
    nl,
    write('Play again (Y/N)? '),
    read(V),
    (V == 'y' ; V == 'Y' ; V == 'n' ; V == 'N'), !
    .

read_play_again(V) :-
    nl,
    nl,
    write('Please enter Y or N.'),
    read_play_again(V)
    .


read_players :-
    nl,
    nl,
    write('Number of human players? '),
    read(N),
    set_players(N)
    .

set_players(0) :- 
    asserta( player(1, computer) ),
    asserta( player(2, computer) ), !
    .

set_players(1) :-
    nl,
    write('Is human playing X or O (X moves first)? '),
    read(M),
    human_playing(M), !
    .

set_players(2) :- 
    asserta( player(1, human) ),
    asserta( player(2, human) ), !
    .

set_players(N) :-
    nl,
    write('Please enter 0, 1, or 2.'),
    read_players
    .


human_playing(M) :- 
    (M == 'x' ; M == 'X'),
    asserta( player(1, human) ),
    asserta( player(2, computer) ), !
    .

human_playing(M) :- 
    (M == 'o' ; M == 'O'),
    asserta( player(1, computer) ),
    asserta( player(2, human) ), !
    .

human_playing(M) :-
    nl,
    write('Please enter X or O.'),
    set_players(1)
    .



% Lancer une série de tests
test :-
    test_initialize,
    test_play,
    test_win_conditions,
    test_board_full,
    write('All tests passed!'), nl.

% Test : Initialisation du tableau
test_initialize :-
    initialize,
    board(B),
    write('Testing board initialization...'), nl,
    output_board, % Affiche le tableau initial
    blank_mark(E),
    forall(member(Row, B), forall(member(Cell, Row), Cell = E)), % Vérifie que toutes les cases sont vides
    write('Board initialized successfully.'), nl.

% Test : Simuler une partie de jeu
test_play :-
    write('Testing game play simulation...'), nl,
    initialize,
    play_test_moves([[1, x], [2, o], [1, x], [2, o], [1, x], [2, o], [1, x]]), % Simule des coups
    board(B),
    output_rows(B), % Affiche l'état du tableau après les mouvements
    win(B, x), % Vérifie si le joueur x a gagné
    write('Game play simulation successful.'), nl.

% Test : Conditions de victoire
test_win_conditions :-
    write('Testing win conditions...'), nl,
    board_empty(B),
    test_horizontal_win(B),
    test_vertical_win(B),
    test_diagonal_win(B),
    write('Win conditions passed.'), nl.

% Test : Plateau plein
test_board_full :-
    write('Testing board full condition...'), nl,
    full_board(B),
    board(B),
    is_board_full(B),
    write('Board full test passed.'), nl.

% Tests spécifiques : Victoire horizontale
test_horizontal_win(B) :-
    write('Testing horizontal win...'), nl,
    set_horizontal_win(B, x, NewBoard), % Place une victoire horizontale pour x
    board(NewBoard),
    win(NewBoard, x),
    write('Horizontal win test passed.'), nl.

% Tests spécifiques : Victoire verticale
test_vertical_win(B) :-
    write('Testing vertical win...'), nl,
    set_vertical_win(B, o, NewBoard), % Place une victoire verticale pour o
    board(NewBoard),
    win(NewBoard, o),
    write('Vertical win test passed.'), nl.

% Tests spécifiques : Victoire diagonale
test_diagonal_win(B) :-
    write('Testing diagonal win...'), nl,
    set_diagonal_win(B, x, NewBoard), % Place une victoire diagonale pour x
    board(NewBoard),
    win(NewBoard, x),
    write('Diagonal win test passed.'), nl.

% Initialisation d'un tableau vide
board_empty([[e, e, e, e, e, e, e],
             [e, e, e, e, e, e, e],
             [e, e, e, e, e, e, e],
             [e, e, e, e, e, e, e],
             [e, e, e, e, e, e, e],
             [e, e, e, e, e, e, e]]).

% Plateau plein pour test
full_board([[x, o, x, o, x, o, x],
            [o, x, o, x, o, x, o],
            [x, o, x, o, x, o, x],
            [o, x, o, x, o, x, o],
            [x, o, x, o, x, o, x],
            [o, x, o, x, o, x, o]]).

% Place une victoire horizontale
set_horizontal_win(Board, Mark, NewBoard) :-
    board_empty(Board),
    replace_row(Board, 6, [Mark, Mark, Mark, Mark, e, e, e], NewBoard).

% Place une victoire verticale
set_vertical_win(Board, Mark, NewBoard) :-
    board_empty(Board),
    replace_column(Board, 1, [Mark, Mark, Mark, Mark, e, e], NewBoard).

% Place une victoire diagonale
set_diagonal_win(Board, Mark, NewBoard) :-
    board_empty(Board),
    replace_diagonal(Board, Mark, NewBoard).

% Remplace une ligne
replace_row(Board, RowIdx, NewRow, NewBoard) :-
    nth1(RowIdx, Board, _, Rest),
    nth1(RowIdx, NewBoard, NewRow, Rest).

% Remplace une colonne
replace_column(Board, ColIdx, NewColumn, NewBoard) :-
    transpose(Board, Transposed),
    replace_row(Transposed, ColIdx, NewColumn, NewTransposed),
    transpose(NewTransposed, NewBoard).

% Remplace une diagonale
replace_diagonal(Board, Mark, NewBoard) :-
    board_empty(Board),
    NewBoard = [[Mark, e, e, e, e, e, e],
                [e, Mark, e, e, e, e, e],
                [e, e, Mark, e, e, e, e],
                [e, e, e, Mark, e, e, e],
                [e, e, e, e, e, e, e],
                [e, e, e, e, e, e, e]].

% Simulation de coups
play_test_moves([]).
play_test_moves([[Col, Mark] | Rest]) :-
    board(B),
    insert_in_column(B, Col, Mark, NewBoard),
    retract(board(_)),
    asserta(board(NewBoard)),
    play_test_moves(Rest).



play(P) :-
    board(B), !,
    write('Current player: '), write(P), nl,
    output_board, !,
    (not(game_over(P, B)) ->
        (make_move(P, B), !,
         next_player(P, P2), !,
         play(P2))
    ;
        write('Game over!'), nl).



%.......................................
% square
%.......................................
% The mark in a square(N) corresponds to an item in a list, as follows:

% square(Board, Row, Col, Value)
% Récupère la valeur de la case à la ligne Row et colonne Col
square(Board, Row, Col, Value) :-
    nth1(Row, Board, Line),      % Récupère la ligne Row
    nth1(Col, Line, Value).      % Récupère la colonne Col dans cette ligne

%.......................................
% win
%.......................................
% Players win by having their mark in one of the following square configurations

win(Board, M) :-
    member(Row, Board),  
    write('Row: '), write(Row), nl,
    consecutive_four(Row, M).    % Vérifie si 4 jetons consécutifs sont présents

win(Board, M) :-
    transpose(Board, Transposed),  % Convertit les colonnes en lignes
    member(Column, Transposed),    % Parcourt chaque colonne (ligne transposée)
    consecutive_four(Column, M).   % Vérifie si 4 jetons consécutifs sont présents

transpose([[]|_], []) :- !.  
transpose(Matrix, [Col|Cols]) :-
    maplist(head_tail, Matrix, Col, RestMatrix),
    transpose(RestMatrix, Cols).

head_tail([H|T], H, T). 

win(Board, M) :-
    diagonals_desc(Board, Diagonals),
    member(Diagonal, Diagonals),
    consecutive_four(Diagonal, M).

win(Board, M) :-
    diagonals_asc(Board, Diagonals),
    member(Diagonal, Diagonals),
    consecutive_four(Diagonal, M).

diagonals_desc(Board, Diagonals) :-
    findall(D, diagonal_desc(Board, D), Diagonals).

diagonal_desc(Board, Diagonal) :-
    nth1(RowIndex, Board, Row),
    nth1(ColIndex, Row, _),
    extract_diagonal_desc(Board, RowIndex, ColIndex, Diagonal).

extract_diagonal_desc(_, RowIndex, ColIndex, []) :-
    RowIndex > 6 ; ColIndex > 7, !.  % Hors limites
extract_diagonal_desc(Board, RowIndex, ColIndex, [Elem|Diagonal]) :-
    nth1(RowIndex, Board, Row),
    nth1(ColIndex, Row, Elem),
    NextRow is RowIndex + 1,
    NextCol is ColIndex + 1,
    extract_diagonal_desc(Board, NextRow, NextCol, Diagonal).


consecutive_four(List, M) :-
    append(_, [M, M, M, M | _], List).


%.......................................
% move
%.......................................
% applies a move on the given board
% (put mark M in square S on board B and return the resulting board B2)
%

move(B,S,M,B2) :-
    set_item(B,S,M,B2)
    .


%.......................................
% game_over
%.......................................
% determines when the game is over
%
game_over(P, B) :-
    game_over2(P, B)
    .

game_over2(_, Board) :-
    is_board_full(Board).


game_over2(P, B) :-
    opponent_mark(P, M),   %%% game is over if opponent wins
    win(B, M)
    .

game_over2(P, B) :-
    blank_mark(E),
    not(square(B,S,E))     %%% game is over if opponent wins
    .

% Vérifie si le tableau est plein (aucune case vide)
is_board_full(Board) :-
    is_list(Board),
    forall(member(Row, Board), is_list(Row)),  % Vérifie que chaque ligne est une liste
    blank_mark(E),
    \+ (member(Row, Board), member(E, Row)).  % Vérifie l'absence de cases vides


%.......................................
% make_move
%.......................................
% requests next move from human/computer, 
% then applies that move to the given board
%

make_move(P, B) :-
    player(P, Type),
    make_move2(Type, P, B, B2),  
    retract(board(_)),           
    asserta(board(B2)).


make_move2(human, P, B, B2) :-
    nl,
    nl,
    write('Player '),
    write(P),
    write(', select a column (1-7): '),
    read(Col),

    valid_column(B, Col),                
    player_mark(P, M),                   
    insert_in_column(B, Col, M, B2),
    !.

make_move2(human, P, B, B2) :-           
    nl,
    nl,
    write('Invalid column. Please select a valid column.'),
    make_move2(human, P, B, B2).


make_move2(computer, P, B, B2) :-
    nl,
    nl,
    write('Computer is thinking about next move...'),
    player_mark(P, M),
    minimax(0, B, M, S, U),
    move(B,S,M,B2),

    nl,
    nl,
    write('Computer places '),
    write(M),
    write(' in square '),
    write(S),
    write('.')
    .


%.......................................
% moves
%.......................................
% retrieves a list of available moves (empty squares) on a board.
%

move(Board, Col, PlayerMark, NewBoard) :-
    insert_in_column(Board, Col, PlayerMark, NewBoard).

insert_in_column(Board, ColIndex, PlayerMark, NewBoard) :-
    reverse(Board, ReversedBoard),  % Commence par le bas (ligne 6 -> ligne 1)
    insert_in_column_reversed(ReversedBoard, ColIndex, PlayerMark, UpdatedReversed),
    reverse(UpdatedReversed, NewBoard).

insert_in_column_reversed([Row | Rest], ColIndex, PlayerMark, [UpdatedRow | Rest]) :-
    nth1(ColIndex, Row, e),              % Trouve la première case vide
    set_item(Row, ColIndex, PlayerMark, UpdatedRow). % Insère la marque
insert_in_column_reversed([Row | Rest], ColIndex, PlayerMark, [Row | UpdatedRest]) :-
    insert_in_column_reversed(Rest, ColIndex, PlayerMark, UpdatedRest).

set_item([_ | Rest], 1, Value, [Value | Rest]).
set_item([X | Rest], Index, Value, [X | UpdatedRest]) :-
    Index > 1,
    NextIndex is Index - 1,
    set_item(Rest, NextIndex, Value, UpdatedRest).


%.......................................
% utility
%.......................................
% determines the value of a given board position
%

valid_column(Board, Col) :-
    Col >= 1,
    Col =< 7,
    nth1(1, Board, FirstRow),   
    nth1(Col, FirstRow, e).    

utility(B,U) :-
    win(B,'x'),
    U = 1, 
    !
    .

utility(B,U) :-
    win(B,'o'),
    U = (-1), 
    !
    .

utility(B,U) :-
    U = 0
    .


%.......................................
% minimax
%.......................................
% The minimax algorithm always assumes an optimal opponent.
% For tic-tac-toe, optimal play will always result in a tie, so the algorithm is effectively playing not-to-lose.

% For the opening move against an optimal player, the best minimax can ever hope for is a tie.
% So, technically speaking, any opening move is acceptable.
% Save the user the trouble of waiting  for the computer to search the entire minimax tree 
% by simply selecting a random square.

minimax(D,[E,E,E, E,E,E, E,E,E],M,S,U) :-   
    blank_mark(E),
    random_int_1n(9,S),
    !
    .

minimax(D,B,M,S,U) :-
    D2 is D + 1,
    moves(B,L),          %%% get the list of available moves
    !,
    best(D2,B,M,L,S,U),  %%% recursively determine the best available move
    !
    .

% if there are no more available moves, 
% then the minimax value is the utility of the given board position

minimax(D,B,M,S,U) :-
    utility(B,U)      
    .


%.......................................
% best
%.......................................
% determines the best move in a given list of moves by recursively calling minimax
%

% if there is only one move left in the list...

best(D,B,M,[S1],S,U) :-
    move(B,S1,M,B2),        %%% apply that move to the board,
    inverse_mark(M,M2), 
    !,  
    minimax(D,B2,M2,_S,U),  %%% then recursively search for the utility value of that move.
    S = S1, !,
    output_value(D,S,U),
    !
    .

% if there is more than one move in the list...

best(D,B,M,[S1|T],S,U) :-
    move(B,S1,M,B2),             %%% apply the first move (in the list) to the board,
    inverse_mark(M,M2), 
    !,
    minimax(D,B2,M2,_S,U1),      %%% recursively search for the utility value of that move,
    best(D,B,M,T,S2,U2),         %%% determine the best move of the remaining moves,
    output_value(D,S1,U1),      
    better(D,M,S1,U1,S2,U2,S,U)  %%% and choose the better of the two moves (based on their respective utility values)
    .


%.......................................
% better
%.......................................
% returns the better of two moves based on their respective utility values.
%
% if both moves have the same utility value, then one is chosen at random.

better(D,M,S1,U1,S2,U2,     S,U) :-
    maximizing(M),                     %%% if the player is maximizing
    U1 > U2,                           %%% then greater is better.
    S = S1,
    U = U1,
    !
    .

better(D,M,S1,U1,S2,U2,     S,U) :-
    minimizing(M),                     %%% if the player is minimizing,
    U1 < U2,                           %%% then lesser is better.
    S = S1,
    U = U1, 
    !
    .

better(D,M,S1,U1,S2,U2,     S,U) :-
    U1 == U2,                          %%% if moves have equal utility,
    random_int_1n(10,R),               %%% then pick one of them at random
    better2(D,R,M,S1,U1,S2,U2,S,U),    
    !
    .

better(D,M,S1,U1,S2,U2,     S,U) :-        %%% otherwise, second move is better
    S = S2,
    U = U2,
    !
    .


%.......................................
% better2
%.......................................
% randomly selects two squares of the same utility value given a single probability
%

better2(D,R,M,S1,U1,S2,U2,  S,U) :-
    R < 6,
    S = S1,
    U = U1, 
    !
    .

better2(D,R,M,S1,U1,S2,U2,  S,U) :-
    S = S2,
    U = U2,
    !
    .



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% OUTPUT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


output_players :- 
    nl,
    player(1, V1),
    write('Player 1 is '),   %%% either human or computer
    write(V1),
    nl,

    nl,
    player(2, V2),
    write('Player 2 is '),   %%% either human or computer
    write(V2), 
    nl,
    nl,
    !
    .


output_winner(B) :-
    win(B, x),
    write('X wins.'),
    !
    .

output_winner(B) :-
    win(B, o),
    write('O wins.'),
    !
    .

output_winner(B) :-
    write('No winner.')
    .


output_board :-
    nl,
    output_column_indices,  % Afficher les indices des colonnes
    board(Board),
    nl,
    output_rows(Board).      % Afficher les lignes


output_column_indices :-
    write('  1   2   3   4   5   6   7'), nl.


output_rows([]) :- nl.  
output_rows([Row | Rest]) :-
    output_row(Row), nl,      % Afficher chaque ligne
    output_rows(Rest).        % Passer à la suivante


output_row(Row) :-
    write('|'),
    output_cells(Row).        % Afficher chaque cellule de la ligne


output_cells([]).             % Fin de la ligne
output_cells([Cell | Rest]) :-
    write(' '), write(Cell), write(' |'),  % Affiche la cellule et un séparateur
    output_cells(Rest).


output_square(B, S) :-
    square(B, S, M),
    write(' '), 
    output_square2(S, M),  
    write(' '), !
    .

output_square2(S, E) :- 
    blank_mark(E),
    write(S), !              %%% Si la case est vide, afficher son index
    .

output_square2(S, M) :- 
    write(M), !              %%% Si la case est marquée, afficher la marque
    .


output_value(D, S, U) :-
    D == 1,
    nl,
    write('Square '),
    write(S),
    write(', utility: '),
    write(U), !
    .

output_value(D, S, U) :- 
    true.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PSEUDO-RANDOM NUMBERS 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%.......................................
% random_seed
%.......................................
% Initialize the random number generator...
% If no seed is provided, use the current time
%

random_seed :-
    random_seed(_),
    !
    .

random_seed(N) :-
    nonvar(N),
% Do nothing, SWI-Prolog does not support seeding the random number generator
    !
    .

random_seed(N) :-
    var(N),
% Do nothing, SWI-Prolog does not support seeding the random number generator
    !
    .

/*****************************************
 OTHER COMPILER SUPPORT
******************************************

arity_prolog___random_seed(N) :-
    nonvar(N),
    randomize(N), 
    !
    .

arity_prolog___random_seed(N) :-
    var(N),
    time(time(Hour,Minute,Second,Tick)),
    N is ( (Hour+1) * (Minute+1) * (Second+1) * (Tick+1)),
    randomize(N), 
    !
    .

******************************************/



%.......................................
% random_int_1n
%.......................................
% returns a random integer from 1 to N
%
random_int_1n(N, V) :-
    V is random(N) + 1,
    !
    .

/*****************************************
 OTHER COMPILER SUPPORT
******************************************

arity_prolog___random_int_1n(N, V) :-
    R is random,
    V2 is (R * N) - 0.5,           
    float_text(V2,V3,fixed(0)),
    int_text(V4,V3),
    V is V4 + 1,
    !
    .

******************************************/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% LIST PROCESSING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

member([V|T], V).
member([_|T], V) :- member(T,V).

append([], L, L).
append([H|T1], L2, [H|T3]) :- append(T1, L2, T3).


%.......................................
% set_item
%.......................................
% Given a list L, replace the item at position N with V
% return the new list in list L2
%

set_item(L, N, V, L2) :-
    set_item2(L, N, V, 1, L2)
        .

set_item2( [], N, V, A, L2) :- 
    N == -1, 
    L2 = []
    .

set_item2( [_|T1], N, V, A, [V|T2] ) :- 
    A = N,
    A1 is N + 1,
    set_item2( T1, -1, V, A1, T2 )
    .

set_item2( [H|T1], N, V, A, [H|T2] ) :- 
    A1 is A + 1, 
    set_item2( T1, N, V, A1, T2 )
    .


%.......................................
% get_item
%.......................................
% Given a list L, retrieve the item at position N and return it as value V
%

get_item(L, N, V) :-
    get_item2(L, N, 1, V)
    .

get_item2( [], _N, _A, V) :- 
    V = [], !,
    fail
        .

get_item2( [H|_T], N, A, V) :- 
    A = N,
    V = H
    .

get_item2( [_|T], N, A, V) :-
    A1 is A + 1,
    get_item2( T, N, A1, V)
    .


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% End of program
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%