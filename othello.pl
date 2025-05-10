% ---------------------------------------------------- */
%
%    D7012E Declarative languages
%    Luleå University of Technology
%
%    Student full name: <Agvan Bedrosian> 
%    Student user id  : <agvbed-0> 
%
/* ------------------------------------------------------- */



%do not chagne the follwoing line!
:- ensure_loaded('play.pl').

%%JAG: Vi kan ändra detta till stupid.pl filen så att play och playgame predicates binder sig till
%% respektive fil!
% :- ensure_loaded('stupid.pl').

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% /* ------------------------------------------------------ */
%               IMPORTANT! PLEASE READ THIS SUMMARY:
%       This files gives you some useful helpers (set &get).
%       Your job is to implement several predicates using
%       these helpers. Feel free to add your own helpers if
%       needed, as long as you write comments (documentation)
%       for all of them. 
%
%       Implement the following predicates at their designated
%       space in this file. You might like to have a look at
%       the file  ttt.pl  to see how the implementations is
%       done for game tic-tac-toe.
%
%          * initialize(InitialState,InitialPlyr).
%          * winner(State,Plyr) 
%          * tie(State)
%          * terminal(State) 
%          * moves(Plyr,State,MvList)
%          * nextState(Plyr,Move,State,NewState,NextPlyr)
%          * validmove(Plyr,State,Proposed)
%          * h(State,Val)  (see question 2 in the handout)
%          * lowerBound(B)
%          * upperBound(B)
% /* ------------------------------------------------------ */







% /* ------------------------------------------------------ */

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% We use the following State Representation: 
% [Row0, Row1 ... Rown] (ours is 6x6 so n = 5 ).
% each Rowi is a LIST of 6 elements '.' or '1' or '2' as follows: 
%    . means the position is  empty
%    1 means player one has a stone in this position
%    2 means player two has a stone in this position. 





% DO NOT CHANGE THE COMMENT BELOW.
%
% given helper: Inital state of the board

initBoard([ [.,.,.,.,.,.], 
            [.,.,.,.,.,.],
	    [.,.,1,2,.,.], 
	    [.,.,2,1,.,.], 
            [.,.,.,.,.,.], 
	    [.,.,.,.,.,.] ]).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%% IMPLEMENT: initialize(...)%%%%%%%%%%%%%%%%%%%%%
%%% Using initBoard define initialize(InitialState,InitialPlyr). 
%%%  holds iff InitialState is the initial state and 
%%%  InitialPlyr is the player who moves first. 


%%JAG: Vi utgår från ttt.pl och gör likadant? bara att vi binder till
%% initBoard som vi har fått sen innan.
initialize(InitialState,1) :-
	initBoard(InitialState).





% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%winner(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define winner(State,Plyr) here.  
%     - returns winning player if State is a terminal position and
%     Plyr has a higher score than the other player 

winner(State, Plyr) :-
    terminal(State),
    (checkStones(State, 1, P1),
    checkStones(State, 2, P2),
    P1 > P2,
    Plyr = 2;
	checkStones(State, 1, P1),
    checkStones(State, 2, P2),
    P2 > P1,
    Plyr = 1).

checkStones(State, Plyr, PlyrResult) :-
	validBoard(State),
    findall([X,Y],(between(0,5,X),between(0,5,Y),get(State,[X,Y],Plyr)),Stones),
    length(Stones, PlyrResult).



validBoard(Board) :-
	length(Board, 6),
	nth0(0, Board, R0), length(R0, 6),
	nth0(1, Board, R1), length(R1, 6),
	nth0(2, Board, R2), length(R2, 6),
	nth0(3, Board, R3), length(R3, 6),
	nth0(4, Board, R4), length(R4, 6),
	nth0(5, Board, R5), length(R5, 6).




% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%tie(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define tie(State) here. 
%    - true if terminal State is a "tie" (no winner) 

tie(State) :-
	terminal(State),
	checkStones(State, 1, P1),
	checkStones(State, 2, P2),
	P1 == P2.




% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%terminal(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define terminal(State). 
%   - true if State is a terminal   

terminal(State) :-  		 	
	moves(1,State,MoveList1),
	moves(2,State,MoveList2),
	MoveList1 == [n],
	MoveList2 == [n].


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%showState(State)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% given helper. DO NOT  change this. It's used by play.pl
%%

showState( G ) :- 
	printRows( G ). 
 
printRows( [] ). 
printRows( [H|L] ) :- 
	printList(H),
	nl,
	printRows(L). 

printList([]).
printList([H | L]) :-
	write(H),
	write(' '),
	printList(L).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%moves(Plyr,State,MvList)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define moves(Plyr,State,MvList). 
%   - returns list MvList of all legal moves Plyr can make in State
%

moves(Plyr, State, [n]) :-
    \+ validmove(Plyr, State, _), !.

moves(Plyr, State, MvList) :-
    findall([X,Y], validmove(Plyr, State, [X,Y]), Unsorted),
    sort(Unsorted, MvList).  % sort ensures top-left to bottom-right



% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%nextState(Plyr,Move,State,NewState,NextPlyr)%%%%%%%%%%%%%%%%%%%%
%% 
%% define nextState(Plyr,Move,State,NewState,NextPlyr). 
%   - given that Plyr makes Move in State, it determines NewState (i.e. the next 
%     state) and NextPlayer (i.e. the next player who will move).
%
%set( Board, NewBoard, [X, Y], Value):
nextState(Plyr, [X,Y], State, NewState, NextPlyr) :-
	set(State,NewBoard,[X,Y],Plyr),

	%% Compared to validmove which only is true
	%% here we actually have to alter states aswell, so we have 
	%% to pass the state along
	%% hmm now that i think about it,
	%%that shouldnt be possible because validmove basically verifies
	%%that a flip has to occur
	flip_south(Plyr, NewBoard, [X, Y], NewBoard1),
    % flip_north(Plyr, NewBoard1, [X, Y], NewBoard2),
    % flip_east(Plyr, NewBoard2, [X, Y], NewBoard3),
    % flip_west(Plyr, NewBoard3, [X, Y], NewBoard4),
    % flip_se(Plyr, NewBoard4, [X, Y], NewBoard5),
    % flip_sw(Plyr, NewBoard5, [X, Y], NewBoard6),
    % flip_ne(Plyr, NewBoard6, [X, Y], NewBoard7),
    % flip_nw(Plyr, NewBoard7, [X, Y], NewState),
	NewState = NewBoard1, % ändra till newState sen tror jag?
	% Why?
	opponent(Plyr, NextPlyr).

%% So we dont have to verify anything here since validmove basically
%% confirms that something already is flippable
%% we still need to verify each stone one at a time though...

% Flip stones in the south direction
flip_south(Plyr, State, [X, Y], NewBoard) :-
    Y1 is Y + 1,
    do_flip_south(Plyr, State, [X, Y1], NewBoard).

% Base case: Stop flipping when we reach the player's stone
do_flip_south(Plyr, State, [X, Y], State) :-
    get(State, [X, Y], Plyr).

% Recursive case: Flip the opponent's stone and continue south
do_flip_south(Plyr, State, [X, Y], NewBoard) :-
    opponent(Plyr, Opp),
    get(State, [X, Y], Opp),
    set(State, TempBoard, [X, Y], Plyr),  % Flip the stone
    Y1 is Y + 1,
    do_flip_south(Plyr, TempBoard, [X, Y1], NewBoard).




% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%validmove(Plyr,State,Proposed)%%%%%%%%%%%%%%%%%%%%
%% 
%% define validmove(Plyr,State,Proposed). 
%   - true if Proposed move by Plyr is valid at State.


%N(North),NE(North-East),E(East),SE(South-East),S(South),SW(South-West),W(West),NW(North-West)

validmove(Plyr, State, [X,Y]) :-
    get(State, [X,Y], '.'),           
    opponent(Plyr, Opp),             
    Y1 is Y + 1,
    get(State, [X,Y1], Opp),          
    go_south(Plyr, State, X, Y1).

go_south(Plyr, State, X, Y) :-
    Y2 is Y + 1,
    get(State, [X,Y2], Plyr).

go_south(Plyr, State, X, Y) :-
    Y2 is Y + 1,
    opponent(Plyr, Opp),  
    get(State, [X,Y2], Opp),
    go_south(Plyr, State, X, Y2).



% Helper function to keep track of player/opp stone during comparison
players([1,2]).
opponent(Plyr, Opp) :-
    players(PLs),
    select(Plyr, PLs, [Opp]).


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%h(State,Val)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define h(State,Val). 
%   - given State, returns heuristic Val of that state
%   - larger values are good for Max, smaller values are good for Min
%   NOTE1. If State is terminal h should return its true value.
%   NOTE2. If State is not terminal h should be an estimate of
%          the value of state (see handout on ideas about
%          good heuristics.

h(State, 100) :-
	winner(State, 1), !.
h(State, -100) :-
	winner(State, 2), !.
h(State, 0) :-
	tie(State), !.
h(_, 0).


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%lowerBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define lowerBound(B).  
%   - returns a value B that is less than the actual or heuristic value
%     of all states.


lowerBound(-101).


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%upperBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define upperBound(B). 
%   - returns a value B that is greater than the actual or heuristic value
%     of all states.

upperBound(101).



% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                       %
%                                                                       %
%                Given   UTILITIES                                      %
%                   do NOT change these!                                %
%                                                                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get(Board, Point, Element)
%    : get the contents of the board at position column X and row Y
% set(Board, NewBoard, [X, Y], Value):
%    : set Value at column X row Y in Board and bind resulting grid to NewBoard
%
% The origin of the board is in the upper left corner with an index of
% [0,0], the upper right hand corner has index [5,0], the lower left
% hand corner has index [0,5], the lower right hand corner has index
% [5,5] (on a 6x6 board).
%
% Example
% ?- initBoard(B), showState(B), get(B, [2,3], Value). 
%. . . . . . 
%. . . . . . 
%. . 1 2 . . 
%. . 2 1 . . 
%. . . . . . 
%. . . . . . 
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], 
%     ['.', '.', 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], 
%     ['.', '.', '.', '.'|...], ['.', '.', '.'|...]]
%Value = 2 
%Yes
%?- 
%
% Setting values on the board
% ?- initBoard(B),  showState(B),set(B, NB1, [2,4], 1),
%         set(NB1, NB2, [2,3], 1),  showState(NB2). 
%
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 2 1 . . 
% . . . . . . 
% . . . . . .
% 
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 1 1 . . 
% . . 1 . . . 
% . . . . . .
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.', 
%1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', '.', '.'|...], ['.', '.',
% '.'|...]]
%NB1 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', '.
%', '.'|...]]
%NB2 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 1, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', 
%'.', '.'|...]]

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% get(Board, Point, Element): get the value of the board at position
% column X and row Y (indexing starts at 0).
% Do not change get:

get( Board, [X, Y], Value) :- 
	nth0( Y, Board, ListY), 
	nth0( X, ListY, Value).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% set( Board, NewBoard, [X, Y], Value): set the value of the board at position
% column X and row Y to Value (indexing starts at 0). Returns the new board as
% NewBoard. Do not change set:

set( [Row|RestRows], [NewRow|RestRows], [X, 0], Value) :-
    setInList(Row, NewRow, X, Value). 

set( [Row|RestRows], [Row|NewRestRows], [X, Y], Value) :-
    Y > 0, 
    Y1 is Y-1, 
    set( RestRows, NewRestRows, [X, Y1], Value). 

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% setInList( List, NewList, Index, Value): given helper to set. Do not
% change setInList:

setInList( [_|RestList], [Value|RestList], 0, Value). 

setInList( [Element|RestList], [Element|NewRestList], Index, Value) :- 
	Index > 0, 
	Index1 is Index-1, 
	setInList( RestList, NewRestList, Index1, Value). 
 

