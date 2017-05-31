/*
 * Josh Post Minesweeper Game
 * CSC344
 * Assignment 3
 */
:- consult('aux').

:- dynamic board/2.
:- op(900, yfx, <-).
Suf <- N :-
   (N == 1 -> Suf = 'st'; N == 2 -> Suf = 'nd';
    N == 3 -> Suf = 'rd'; Suf = 'th').

% checked('_').
% unchecked('.').
mark('_').

checked(X, Y) :-
   located_at(visible,X,Y,B), (mark(B); number(B)).

unchecked(X, Y) :-
   \+ checked(X,Y).

bomb('b').
filter(_, [], []).
filter(P, [A |As], Cs) :-
    (
       call(P, A) -> B = [A]
    ;
       B = []
    )
    , filter(P, As, Bs), append(B, Bs, Cs).

maxX(10).
maxY(10).
legit([A,B]) :-
   maxX(MaxX), maxY(MaxY),
   1 =< A, A =< MaxX, 1 =< B, B =< MaxY.

board(visible,
[
   ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.'],
   ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.'],
   ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.'],
   ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.'],
   ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.'],
   ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.'],
   ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.'],
   ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.'],
   ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.'],
   ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
]).

board(hidden,
[
   ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.'],
   ['.', '.', '.', '.', '.', '.', '.', 'b', '.', '.'],
   ['.', '.', 'b', '.', '.', '.', '.', '.', '.', '.'],
   ['b', '.', 'b', '.', 'b', '.', 'b', '.', '.', '.'],
   ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.'],
   ['.', '.', '.', '.', 'b', '.', '.', '.', '.', '.'],
   ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.'],
   ['b', 'b', '.', '.', '.', '.', 'b', 'b', '.', '.'],
   ['.', '.', 'b', '.', '.', '.', '.', 'b', '.', '.'],
   ['.', '.', '.', '.', '.', '.', '.', '.', '.', '.']
]).

display_board(Which) :-
   board(Which,Board),
   do_display_board(Board).
do_display_board([]) :-
   nl, nl.
do_display_board([Row|Rows]) :-
   display_row(Row), nl,
   do_display_board(Rows).
display_row([]).
display_row([X|Xs]) :-
   write(' '), write(X), write(' '),
   display_row(Xs).

% returns character at location X,Y
located_at(Which, X, Y, C) :-
   board(Which,Board),
   located_in_row(X,Y,1,C,Board).

located_in_row(_,_,_,_,[]) :- fail.
located_in_row(X,Y,X,C,[Row|_]) :-
   located_in_col(Y,1,C,Row).
located_in_row(X,Y,M,C,[_|Rows]) :-
   N is M + 1,
   located_in_row(X,Y,N,C,Rows).

located_in_col(_,_,_,[]) :- fail.
located_in_col(Y,Y,C,[C|_]).
located_in_col(Y,M,C,[_|Cols]) :-
   N is M + 1,
   located_in_col(Y,N,C,Cols).

/*
 * place_at(+Which, +X, +Y, +Char)
 * Which is visible or hidden
 */
place_at(Which,X,Y,Char) :-
   board(Which,Board),
   place_in_row(X,Y,1,Char,Board,NewBoard),
   retract(board(Which,Board)),
   assert(board(Which,NewBoard)).

place_in_row(_,_,_,_,[],_) :- fail.
place_in_row(X,Y,X,C,[Row|Rest],[NewRow|Rest]) :-
   place_in_col(Y,1,C,Row,NewRow).
place_in_row(X,Y,M,C,[Row|Rows],[Row|NewRows]) :-
   N is M + 1,
   place_in_row(X,Y,N,C,Rows,NewRows).

place_in_col(_,_,_,[],_) :- fail.
place_in_col(Y,Y,C,[D|Cols],NewCols) :-
    !, (
        D == '.', NewCols = [C|Cols];
        NewCols = [D|Cols]
       ).
place_in_col(Y,M,C,[Col|Cols],[Col|NewCols]) :-
   N is M + 1,
   place_in_col(Y,N,C,Cols,NewCols).


retrieve(Prompt, Term) :-
   repeat,
      (
      write(Prompt),
      read_sentence(Sentence),
      % write_sentence(Sentence), nl,
      butlast(Sentence, Sentenc),
      % write(Sentenc), nl,
      phrase(sentence(Loc), Sentenc, []),
         (
         Loc == 'quit', write('quitting ...'), abort
         ;
         Term = Loc % note: in form [X, Y]
         )
      )
   .

play :-
   play(0).

play(M) :-
   N is M + 1,
   Suf <- N,
   display_board(visible), nl,
   format("Your ~d~a move~n", [N,Suf]),
   retrieve('Coordinates? ', [A,B]),
   !, probe(A,B),
   play(N).

/*
 * Be able to do this on exam 2
 */
count(_, [], 0).
count(X, [X|Ys], Z) :- count(X, Ys, W), Z is 1+W.
count(X, [_|Ys], W) :- count(X, Ys, W).

neighbors(X, Y, Nbrs) :-
   NWX is X-1, NWY is Y-1,
   NEX is X-1, NEY is Y+1,
   WX  is X,   WY  is Y-1,
   EX  is X,   EY  is Y+1,
   SWX is X+1, SWY is Y-1,
   SEX is X+1, SEY is Y+1,
   NX is X-1, NY is Y,
   SX is X+1, SY is Y,

   Nbrs_unfiltered =
   [[NWX, NWY],
    [NEX, NEY],
    [WX,  WY],
    [EX,  EY],
    [SWX, SWY],
    [SEX, SEY],
    [NX,  NY],
    [SX,  SY]],
   filter(legit, Nbrs_unfiltered, Nbrs).


:- style_check(-singleton).

has_bomb(X,Y) :-
   located_at(hidden, X, Y, 'b').

bomb_count([X,Y], Count) :-
   has_bomb(X,Y) -> 
   Count is 1; Count is 0.

bomb_count_neighbors([X,Y], Count) :-
   neighbors(X,Y, Nbrs),
   N = Nbrs,
   sum_ns(N,P),
   Count is P.

sum_ns([], 0).
sum_ns([H|T], Sum) :-
   sum_ns(T, Rest),
   Y = H,
   bomb_count(Y,Count),
   Sum is Count + Rest.

expose([X, Y]) :-
   bomb_count_neighbors([X,Y],Count),
   C is Count,
(  has_bomb(X,Y) -> explode(X,Y)
;  C = 0 -> place_at(visible,X,Y,'_')
;  place_at(visible,X,Y,C)
).
   
explode(X,Y) :-
   place_at(visible,X,Y,'*'),
   write('KABOOM! You lose!'),nl,
   display_board(visible),nl,
   write('quitting ...'), abort.

probe(X, Y) :-
   expose([X,Y]),
   located_at(visible,X,Y,L),
   M = L,
   M = '_' ->
   neighbors(X, Y, Nbrs),
   N = Nbrs,
   pnbrs(N);true.

pnbrs([]).
pnbrs([H|T]) :-
   pnbrs(T),
   [X|A] = H,
   [Y|B] = A,
   located_at(visible,X,Y,L),
   M = L,
   M = '.' ->
   probe(X,Y);true.
   

:- style_check(+singleton).
