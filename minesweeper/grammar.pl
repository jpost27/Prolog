% File:  grammar.pl
%   - grammar for Minesweeper game

sentence(quit) --> [quit].
sentence([Y, X]) --> [probe], [X], [Y], {number(X), number(Y)}.
