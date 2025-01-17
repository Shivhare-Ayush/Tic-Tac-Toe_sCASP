:- use_module(library(scasp)).
:- style_check(-discontiguous).
:- style_check(-singleton).
:- set_prolog_flag(scasp_unknown, fail).

% Board positions and players
valid_pos(0). valid_pos(1). valid_pos(2).
valid_player(x). valid_player(o).

% Initial game state
%      X                O
%┌────────────── •✧• ──────────────┐
  moved(0, 2, x).    moved(2, 1, o).
  moved(1, 1, x).    moved(2, 0, o).


%└────────────── •✧• ──────────────┘
% Move definition using s(CASP) compatible negation
move(Row, Column, Player) :- 
    valid_pos(Column),
    valid_pos(Row),
    valid_player(Player),
    not occupied(Row, Column).

tile(Row_num, Col_num, Tile_val):- 
    valid_pos(Row_num),
    valid_pos(Col_num),
    Tile_val .=. (3 * Row_num) + (Col_num+1).

moved(tile(Row, Column, Tile), Player) :- 
    tile(Row, Column, Tile),
    moved(Row, Column, Player).

move(tile(Row, Column, Tile), Player) :-
    tile(Row, Column, Tile),
    move(Row, Column, Player).
% Occupied position
occupied(Row, Column) :- moved(Row, Column, _).

% Win conditions
win(Player) :- row_win(Player).
win(Player) :- column_win(Player).
win(Player) :- diagonal_win(Player).

row_win(Player) :-
    valid_pos(Row),
    moved(Row, 0, Player),
    moved(Row, 1, Player),
    moved(Row, 2, Player).


column_win(Player) :-
    valid_pos(Column),
    moved(0, Column, Player),
    moved(1, Column, Player),
    moved(2, Column, Player).

diagonal_win(Player) :-
    moved(0, 0, Player),
    moved(1, 1, Player),
    moved(2, 2, Player).

diagonal_win(Player) :-
    moved(0, 2, Player),
    moved(1, 1, Player),
    moved(2, 0, Player).

winnable(Row, Column, Player) :- row_winnable(Row, Column, Player).
winnable(Row, Column, Player) :- column_winnable(Row, Column, Player).
winnable(Row, Column, Player) :- diagonal_winnable(Row, Column, Player).
winnable(Row, Column, Player) :- dw2(Row, Column, Player).

diagonal_winnable(Row, Row, Player) :-
    moved(X1, X1, Player),
    moved(X2, X2, Player),
    not equal(X1, X2),
    move(Row, Row, Player).

valid_diagonal(3). valid_diagonal(5). valid_diagonal(7). 
dw2(Row, Column, Player) :-
    moved(tile(R1, C1, Tile1), Player),
    valid_diagonal(Tile1),
    moved(tile(R2, C2, Tile2), Player),
    valid_diagonal(Tile2),
    not equal(Tile1, Tile2),
    Tile3 .=. 3 + 5 + 7 - Tile1 - Tile2,
    move(tile(Row, Column, Tile3), Player).
%The dw2 call doesnt work but if you copy the inside into prompt it works

row_winnable(Row, Column, Player) :- 
    valid_pos(Row),
    moved(Row, X1, Player),
    moved(Row, X2, Player),
    not equal(X1, X2), 
    move(Row, Column, Player).

column_winnable(Row, Column, Player) :-
    valid_pos(Column),
    moved(X1, Column, Player),
    moved(X2, Column, Player),
    not equal(X1, X2),
    move(Row, Column, Player).
    
% Game state
game_over :- win(_).
game_over :- board_full.

equal(X1, X2) :- X1.=.X2.

best_move(Row, Column, o) :- best_move_win(Row, Column, o).
best_move(Row, Column, o) :- best_move_block(Row, Column, o).
best_move(Row, Column, o) :- best_move_place(Row, Column, o).

best_move_win(Row, Column, o) :-
    not game_over,
    winnable(Row, Column, o).

best_move_block(Row, Column, o) :-
    not next_move1(Row, Column, o),
    winnable(Row, Column, x).

best_move_place(Row, Column, o) :-
    not next_move2(R, C, o),
    not next_move1(R, C, o),
    move(Row, Column, o).


board_full :-
    occupied(0,0), occupied(0,1), occupied(0,2),
    occupied(1,0), occupied(1,1), occupied(1,2),
    occupied(2,0), occupied(2,1), occupied(2,2).

false :-
    move(Row, Column, Player),
    moved(Row, Column, _).
% Example queries
#show move/3.
#show win/1.
#show game_over/0.
?- best_move(X, Y, o). %computer goes second
?- move(X, Y, o). %can also do x 
?- game_over.
?- win(X). %has anyone won



